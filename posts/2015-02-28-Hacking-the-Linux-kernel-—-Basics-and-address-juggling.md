---
title: Hacking the Linux kernel — Basics and address juggling
description: A introduction to kernel module developing by writing a kernel hack which hijacks the sys_newuname call of the kernel.
tags: C Linux Kernel
---

Lately I have come to the point where I had to ask myself: How is this kernel thing really working. After some days of thinking about what could I implement there no idea has come up. (Ok, ok there was: a simple kernel network package filter) I decided to take the vicious way: A kernel module which should be able to hide himself from seen from inside the system + a remote shell started by a "magic" packet.

__In this post I'm going to explain the basics of kernel modules and how to hook the sys_newuname function without the use of the sys_call_table__

<br/>

###Prepare the development environment

> The code written in this post will only work on x86_64 systems.

Because we play around with the kernel it does not make sense to play with the currently running system. The best way is to use a virtual machine where you install all the stuff you think you need. For me it is VirtualBox because snapshot handling is really easy. Usage of snapshots makes a lot things slightly more easy. First of all, just prepare your testing setup (login, open terminal windows which you need and so on..). Save this state and load the kernel module. If all works fine, great! But if not and the kernel is in a dirty state or even crashes just reset to the snapshot.
You don't even install an editor because you can mount the filesystem where the source is located from the host.

Things you should install on the virtualised system:

> - kernel-devel kernel-headers _(Depends on your linux distribution)_
> - build tools _(make and so on)_
> - gcc _(for compiling the kernel)_
> - nasm _(To generate shellcode)_

<br/>

###Upon a time long time, there was a kernel module

The general way of extending the kernel is by writing a kernel module. Setup a `Makefile` which builds module against any installed kernel is pretty easy:

```
obj-m += mantis.o
mantis-objs := module.o hijack.o
KERNELPATH := $$(shell uname -r)

all:
	make -C /lib/modules/$${KERNELPATH}/build M=$$(PWD) modules

clean:
	make -C /lib/modules/$${KERNELPATH}/build M=$$(PWD) clean
```

Easy, right? This will build a module against your running kernel

> __Note:__ `<your-module-name>-objs` is an easy way if adding more other files to the kernel module


Every kernel module needs to implement two functions, a module init function and a module release function. A module looks like:

```{.c .numberLines}
#include <linux/module.h>
#include <linux/init.h>

#include <linux/kernel.h> // printk priorities

MODULE_AUTHOR("Felix Schnizlein"); //optional

// If you want to see printk messages in dmesg you
// need to set a propiate licence
MODULE_LICENSE("GPL");

static int __init mymodule_init(void) {

  // initialize the module
  
  printk(KERN_INFO "Loading mymodule...\n");
  
  return 0;
}


static void __exit mymodule_release(void) {

  // cleanup!
  
  printk(KERN_INFO "Unloading mymodule");
  
  return 0;

}

module_init(mymodule_init);
module_exit(mymodule_release);

```
What's happening should be fairly obvious. If something is going wrong, you should return any __negative__ number.

> __Hint:__ If you want to know more about return values, see manpages `man init_module` and `man delete_module`

You might notice the `__init` and `__exit` flag. `__init` marks the function to be removed from the kernel after initialization. Clearly this function will never be called again. `__exit` is a little bit more complicated. This flag marks the function to be removed if either the kernel does not support kernel unloading or the module is marked as kernel builtin.

One thing about `printk` as you already thought, it behaves like `printf` with one exception. You can mark the message with different information levels (this is optional you don't need to set one).

All possible levels are defined here: [kern_levels.h](http://lxr.free-electrons.com/source/include/linux/kern_levels.h)

> __Hint:__ You can change the log level, which is printed to dmesg dynamically by:
  `echo "<loglevel num>" > /proc/sys/kernel/printk`

<br/>

### Ready to juggle!

Now let's hook the uname of the virtualised system.

> __Note:__ Now this is the part where post leaves the informational part of kernel developing. Never think about using some of this technique in any serious kernel module.

The kernel holds every function which is defined in kernel space (sure the kernel needs to know where to call any function). You can look up the memory space by typing:

    $$ cat /proc/kallsyms
    
or if this feature is disabled (CONFIG_KALLSYMS not set)

    $$ cat /boot/System.map-$$(uname -r)

Every loaded function is listed here. The function we seek `sys_uname` as well

    $$ cat /proc/kallsyms | grep sys_newuname
    3680:ffffffff820acf80 T sys_newuname
    

The goal is to overwrite this address and execute our uname function.

But searching the address manually is pedestrian. Fortunately, there is API in the kernel. In `linux/kallsyms.h` are helper function which can look up the address you want.

> __Note:__ If `CONFIG_KALLSYMS` is disabled the `linux/kallsysms.h` header defines a stub function which does nothing. There is an alternative way by searching the sys_call_table by brute force and using it. See [memset's blog about finding the sys_call_table on 2.6.x kernels](https://memset.wordpress.com/2011/01/20/syscall-hijacking-dynamically-obtain-syscall-table-address-kernel-2-6-x/)

Now we are going to obtain the address:

```{.c .numberLines}
#include <linux/kallsyms.h>

void *sys_newuname = NULL;

static int __init mymodule_init(void) {

  sys_newuname = (void*)kallsyms_lookup_name("sys_newuname");
  
  if (!sys_uname) {
     printk(KERN_DEBUG "Could not load address of sys_newuname\n");
     
     return -1;
  }
  
  printk(KERN_INFO "Address of sys_newuname is 0x%16lX\n", (unsigned long)sys_newuname);

  return 0;
}
```

Tada! Easy right?

## Hooking uname

Now we know the address, but now? We are going to replace a few bytes of the actual uname code and replace it with a basic jump instruction. The easiest way is to prepare a shellcode snippet which jumps to our function.

This means:
 
  1. Create a shellcode
  2. Insert the address of hook function to the shellcode
  3. Copy original instruction from `sys_newuname` address to a variable
  4. Copy shellcode to `sys_newuname` address
  5. hooked!
 
First the function we want to overwrite has the definition:

```{.c}
asmlinkage long sys_newuname(struct new_utsname __user *name);
```
    
Our hook function, therefore should look the same. (`struct new_utsname` is defined in `linux/utsname.h`)

Okay, we know the address we know how to define our `sys_newuname` function, next we generate the shellcode.
Shellcode is just the stringified representation of opcodes which are interpreted by the cpu. Every assembler command has one or more corresponding opcodes.

We need shellcode which jumps to a _absolute_ address.

In assembler this looks fairly easy:

```{.asm}
  mov rax, 0x01234567890123456
  jmp rax
```

We compile this to an object file and disassemble the generated code

    $$ nasm jump.asm -f elf64
    $$ objdump -d jump.o
    
    test.o:     file format elf64-x86-64

    Disassembly of section .text:

    0000000000000000 <.text>:
       0:	48 b8 56 34 12 89 67 	movabs $$0x123456789123456,%rax
       7:	45 23 01 
       a:	ff e0                	jmpq   *%rax

    
This displays the bytecode. The shellcode looks like:

    #define JUMP_CODE "\x48\xb8\x00\x00\x00\x00\x00\x00\x00\x00\xff\xe0"

I just replaced the 0x0123456789123456 with zeros. It shows, that the first two bytes are actually the `mov` opcode and the last two the `jmp` instruction.

This means we need to insert our 8 byte wide address (a `unsigned long`) after 2 bytes. To hook and unhook dynamically the original instruction are saved too. 

```{.c .numberLines}
#define JUMP_CODE "\x48\xb8\x00\x00\x00\x00\x00\x00\x00\x00\xff\xe0"
#define JUMP_SIZE 12
#define JUMP_OFFSET 2 // we want to insert the address after 2 bytes

void *sys_newuname = NULL;

unsigned char jump[JUMP_SIZE];
unsigned char original[JUMP_SIZE];

asmlinkage long hooked_sys_newuname(struct new_utsname __user *name) {

  // ...
  
  return 0;
}

void init_hook() {

  memcpy(jump, JUMP_CODE, JUMP_SIZE);

  *(unsigned long*)&jump[JUMP_OFFSET] = (unsigned long)hooked_sys_newuname;

  memcpy(original, sys_newuname, JUMP_SIZE);
  
}

```
Now `jump` and `original` are initialized. To finally hook, write protection needs to be disabled. Because most systems are multicore nowdays, the module needs to take care that no other process/thread interferes with the hooking mechanism too.

To disable _write protection_ the __16th__ bit of the `x86` control register needs to be cleared. ([More information about the CR0 register](http://en.wikipedia.org/wiki/Control_register#CR0))

To make sure no other process interferes preemption is disabled.

```{.c .numberLines}
#include <linux/preempt.h>

inline unsigned long disable_wp(void)
{
  unsigned long cr0;
  
  preempt_disable();

  cr0 = read_cr0();
  write_cr0(cr0 & ~X86_CR0_WP);
  
  return cr0;
}

inline void restore_wp(unsigned long cr0)
{
  write_cr0(cr0);
  preempt_enable();
}

```

The last thing to write a `hook` and `unhook` function.
Here is the complete code:

```{.c .lineNumbers}
#include <linux/module.h>
#include <linux/init.h>
#include <linux/kallsyms.h>
#include <linux/utsname.h>

#include <linux/slab.h>
#include <linux/preempt.h>

#define JUMP_SIZE 12
#define JUMP_CODE "\x48\xb8\x00\x00\x00\x00\x00\x00\x00\x00\xff\xe0"
#define JUMP_OFFSET 2

MODULE_LICENSE("GPL");

void *sys_newuname = NULL;

unsigned char jump[JUMP_SIZE];
unsigned char original[JUMP_SIZE];

unsigned long cr0;

/**
 * disable write protection and disable preemtation
 */
inline unsigned long disable_wp(void)
{
  unsigned long reg;

  preempt_disable();

  reg = read_cr0();
  write_cr0(reg & ~X86_CR0_WP);

  return reg;
}

/** 
 * restore saved state of cr0 register and renable preemptation
 */
inline void restore_wp(unsigned long reg)
{
  write_cr0(reg);
  preempt_enable();
}

/**
 * activate hook
 */
void hook(void)
{
  cr0 = disable_wp();
  
  memcpy(sys_newuname, (void *)jump, JUMP_SIZE);
  
  restore_wp(cr0);
}


/**
 * disable hook
 */
void unhook(void)
{
  cr0 = disable_wp();
  
  memcpy(sys_newuname, (void *)original, JUMP_SIZE);
  
  retstore_wp(cr0);
}


/**
 * hooked uname function
 */
asmlinkage long hooked_sys_newuname(struct new_utsname __user *name)
{
  long ret;
  long (*real_newuname)(struct new_utsname __user *);
  
  real_uname = (long (*)(struct new_utsname __user *)sys_newuname;
  
  unhook();
  ret = real_newuname(name);
  hook();
  
  // name->sysname has already intialized to char[40];
  strncpy(name->sysname, "hooked Linux!!", 14); 
  
  return ret;
}

/**
 * initialize data structures
 */
void init_hook()
{

  memcpy(jump, JUMP_CODE, JUMP_SIZE);

  *(unsigned long*)&jump[JUMP_OFFSET] = (unsigned long)hooked_sys_newuname;

  memcpy(original, sys_newuname, JUMP_SIZE);
}


/**
 * Initialize the module
 */
static int __init mymodule_init(void)
{

  sys_newuname = (void*)kallsyms_lookup_name("sys_newuname");
  
  if (!sys_uname) {
     printk(KERN_DEBUG "Could not load address of sys_newuname\n");
     
     return -1;
  }
  
  hook_init();
  hook();
  
  return 0;
}


/**
 * cleanup module
 */
static void __exit mymodule_exit(void)
{

  unhook();
}

module_init(mymodule_init);
module_exit(mymodule_exit);

```
<br/>

> __Note:__ This is a quick sketch. Encapsulate the data in a own type (e.g. ksym_hook_t) would be really nice!

And hooked!

    $$ uname
    Linux
    $$ make
    $$ insmod mymodule.ko
    $$ uname
    hooked Linux!!









