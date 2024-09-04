

%include "pm.inc"

; 4个任务页目录地址
PageDirBase0    equ 200000h     ; 任务0的页目录开始地址: 2M
PageTblBase0    equ 201000h     ; 任务0的页表开始地址: 2M + 4K

PageDirBase1    equ 210000h     ; 任务1的页目录开始地址: 2M + 64K
PageTblBase1    equ 211000h     ; 任务1的页表开始地址: 2M + 64K + 4K

PageDirBase2    equ 220000h     ; 任务2的页目录开始地址: 2M + 128K
PageTblBase2    equ 221000h     ; 任务2的页表开始地址: 2M + 128K + 4K

PageDirBase3    equ 230000h     ; 任务3的页目录开始地址: 2M + 192K
PageTblBase3    equ 231000h     ; 任务3的页表开始地址: 2M + 192K + 4K

org 0100h; 设置当前汇编地址为0100h
    jmp LABEL_BEGIN ; 无条件跳转到标签 LABEL_BEGIN


; 定义 GDT（全局描述符表）
[SECTION .gdt]
;                                      段基址,       段界限     , 属性
LABEL_GDT:		Descriptor	       0,                 0, 0										; 空描述符
LABEL_DESC_NORMAL:	Descriptor	       0,            0ffffh, DA_DRW								; Normal 描述符，允许读写
LABEL_DESC_FLAT_C:	Descriptor             0,           0fffffh, DA_CR | DA_32 | DA_LIMIT_4K	; 0 ~ 4G
LABEL_DESC_FLAT_RW:	Descriptor             0,           0fffffh, DA_DRW | DA_LIMIT_4K			; 0 ~ 4G
LABEL_DESC_CODE32:	Descriptor	       0,  SegCode32Len - 1, DA_CR | DA_32						; 非一致代码段, 32
LABEL_DESC_CODE16:	Descriptor	       0,            0ffffh, DA_C								; 非一致代码段, 16
LABEL_DESC_DATA:	Descriptor	       0,	DataLen - 1, DA_DRW									; 数据段，允许读写
LABEL_DESC_STACK:	Descriptor	       0,        TopOfStack, DA_DRWA | DA_32					; 堆栈段，32位
LABEL_DESC_VIDEO:	Descriptor	 0B8000h,            0ffffh, DA_DRW + DA_DPL3					; 显存段，允许读写，特权级3

; 任务状态段描述符
LABEL_DESC_TSS0: 	Descriptor 			0,          TSS0Len-1, DA_386TSS	   ;TSS0
LABEL_DESC_TSS1: 	Descriptor 			0,          TSS1Len-1, DA_386TSS	   ;TSS1
LABEL_DESC_TSS2: 	Descriptor 			0,          TSS2Len-1, DA_386TSS	   ;TSS2
LABEL_DESC_TSS3: 	Descriptor 			0,          TSS3Len-1, DA_386TSS	   ;TSS3

; 四个任务的本地描述符表描述符
LABEL_TASK0_DESC_LDT:    Descriptor         0,   TASK0LDTLen - 1, DA_LDT
LABEL_TASK1_DESC_LDT:    Descriptor         0,   TASK1LDTLen - 1, DA_LDT
LABEL_TASK2_DESC_LDT:    Descriptor         0,   TASK2LDTLen - 1, DA_LDT
LABEL_TASK3_DESC_LDT:    Descriptor         0,   TASK3LDTLen - 1, DA_LDT

; GDT 结束
GdtLen		equ	$ - LABEL_GDT	; GDT长度
GdtPtr		dw	GdtLen - 1	; GDT界限
		dd	0		; GDT基地址

; GDT 选择子

; 定义 GDT 中各个描述符的选择子偏移量

SelectorNormal  equ LABEL_DESC_NORMAL   - LABEL_GDT     ; Normal 描述符的选择子
SelectorFlatC   equ LABEL_DESC_FLAT_C   - LABEL_GDT     ; 0 ~ 4G 代码段的选择子
SelectorFlatRW  equ LABEL_DESC_FLAT_RW  - LABEL_GDT     ; 0 ~ 4G 数据段的选择子
SelectorCode32  equ LABEL_DESC_CODE32   - LABEL_GDT     ; 非一致代码段，32位的选择子
SelectorCode16  equ LABEL_DESC_CODE16   - LABEL_GDT     ; 非一致代码段，16位的选择子
SelectorData    equ LABEL_DESC_DATA     - LABEL_GDT     ; Data 段的选择子
SelectorStack   equ LABEL_DESC_STACK    - LABEL_GDT     ; Stack 段的选择子
SelectorVideo   equ LABEL_DESC_VIDEO    - LABEL_GDT     ; 显存段的选择子
; 四个任务段的选择子
SelectorTSS0    equ LABEL_DESC_TSS0     - LABEL_GDT     ; TSS0 的选择子
SelectorTSS1    equ LABEL_DESC_TSS1     - LABEL_GDT     ; TSS1 的选择子
SelectorTSS2    equ LABEL_DESC_TSS2     - LABEL_GDT     ; TSS2 的选择子
SelectorTSS3    equ LABEL_DESC_TSS3     - LABEL_GDT     ; TSS3 的选择子
SelectorLDT0    equ LABEL_TASK0_DESC_LDT   - LABEL_GDT   ; 任务0的本地描述符表的选择子
SelectorLDT1    equ LABEL_TASK1_DESC_LDT   - LABEL_GDT   ; 任务1的本地描述符表的选择子
SelectorLDT2    equ LABEL_TASK2_DESC_LDT   - LABEL_GDT   ; 任务2的本地描述符表的选择子
SelectorLDT3    equ LABEL_TASK3_DESC_LDT   - LABEL_GDT   ; 任务3的本地描述符表的选择子

; END of [SECTION .gdt]


; LDT 和任务段定义
; ---------------------------------------------------------------------------------------------
; 定义任务
DefineTask 0, "VERY", 20, 0Ch
DefineTask 1, "LOVE", 20, 0Fh
DefineTask 2, "HUST", 20, 0Ch
DefineTask 3, "MRSU", 20, 0Fh
; END of LDT 和任务段定义


; IDT
; ---------------------------------------------------------------------------------------------
[SECTION .idt]
ALIGN	32
[BITS	32]
LABEL_IDT:
; 门                          目标选择子,            偏移, DCount, 属性
; 使用宏 %rep 和 %endrep 循环定义32个中断门描述符，这些描述符均指向 SpuriousHandler
%rep 32
				Gate	SelectorCode32, SpuriousHandler,      0, DA_386IGate
%endrep
.020h:			Gate	SelectorCode32,    ClockHandler,      0, DA_386IGate
; 使用宏 %rep 和 %endrep 循环定义95个中断门描述符，这些描述符均指向 SpuriousHandler
%rep 95
				Gate	SelectorCode32, SpuriousHandler,      0, DA_386IGate
%endrep
.080h:			Gate	SelectorCode32,  UserIntHandler,      0, DA_386IGate

IdtLen		equ	$ - LABEL_IDT	; IDT 长度
IdtPtr		dw	IdtLen - 1		; IDT 段界限
			dd	0				; IDT 基地址, 待设置
; END of [SECTION .idt]



; 数据段
; ---------------------------------------------------------------------------------------------
[SECTION .data1]	 ; 数据段
ALIGN	32
[BITS	32]
LABEL_DATA:
; 实模式下使用这些符号
; 字符串
_szPMMessage:			db	"In Protect Mode now!!!!!", 0Ah, 0Ah, 0	; 进入保护模式后显示此字符串
_szMemChkTitle:			db	"BaseAddrL BaseAddrH LengthLow LengthHigh   Type", 0Ah, 0	; 进入保护模式后显示此字符串
_szRAMSize			db	"RAM size:", 0
_szReturn			db	0Ah, 0
_szReadyMessage:			db	"Task4", 0
; 变量
_wSPValueInRealMode		dw	0
_dwMCRNumber:			dd	0	; Memory Check Result
_dwDispPos:			dd	(80 * 6 + 0) * 2	; 屏幕第 6 行, 第 0 列。
_dwMemSize:			dd	0
_ARDStruct:			; Address Range Descriptor Structure
	_dwBaseAddrLow:		dd	0
	_dwBaseAddrHigh:	dd	0
	_dwLengthLow:		dd	0
	_dwLengthHigh:		dd	0
	_dwType:		dd	0
_PageTableNumber:		dd	0
_SavedIDTR:			dd	0	; 用于保存 IDTR
				dd	0
_SavedIMREG:			db	0	; 中断屏蔽寄存器值
_MemChkBuf:	times	256	db	0

%define tickTimes  30;
_RunningTask:			dd	0
_TaskPriority:			dd	16*tickTimes, 10*tickTimes, 8*tickTimes, 6*tickTimes
_LeftTicks:			dd	0, 0, 0, 0

; 保护模式下使用这些符号
szPMMessage		equ	_szPMMessage	- $$
szMemChkTitle		equ	_szMemChkTitle	- $$
szRAMSize		equ	_szRAMSize	- $$
szReturn		equ	_szReturn	- $$
szReadyMessage  equ _szReadyMessage - $$
dwDispPos		equ	_dwDispPos	- $$
dwMemSize		equ	_dwMemSize	- $$
dwMCRNumber		equ	_dwMCRNumber	- $$
ARDStruct		equ	_ARDStruct	- $$
	dwBaseAddrLow	equ	_dwBaseAddrLow	- $$
	dwBaseAddrHigh	equ	_dwBaseAddrHigh	- $$
	dwLengthLow	equ	_dwLengthLow	- $$
	dwLengthHigh	equ	_dwLengthHigh	- $$
	dwType		equ	_dwType		- $$
MemChkBuf		equ	_MemChkBuf	- $$
SavedIDTR		equ	_SavedIDTR	- $$
SavedIMREG		equ	_SavedIMREG	- $$
PageTableNumber		equ	_PageTableNumber- $$
; 任务相关变量
RunningTask     equ _RunningTask - $$
TaskPriority    equ _TaskPriority - $$
LeftTicks       equ _LeftTicks - $$
DataLen			equ	$ - LABEL_DATA
; END of [SECTION .data1]

; 全局堆栈段
; ---------------------------------------------------------------------------------------------
[SECTION .gs]
ALIGN	32
[BITS	32]
LABEL_STACK:
	times 512 db 0

TopOfStack	equ	$ - LABEL_STACK - 1
; END of [SECTION .gs]


; 16位代码段
; ---------------------------------------------------------------------------------------------
[SECTION .s16]
[BITS	16]
LABEL_BEGIN:
	; 准备工作
	mov	ax, cs
	mov	ds, ax
	mov	es, ax
	mov	ss, ax
	mov	sp, 0100h
	mov	[LABEL_GO_BACK_TO_REAL+3], ax
	mov	[_wSPValueInRealMode], sp
	; 得到内存数
	mov	ebx, 0
	mov	di, _MemChkBuf
.loop:
	mov	eax, 0E820h
	mov	ecx, 20
	mov	edx, 0534D4150h
	int	15h
	jc	LABEL_MEM_CHK_FAIL
	add	di, 20
	inc	dword [_dwMCRNumber]
	cmp	ebx, 0
	jne	.loop
	jmp	LABEL_MEM_CHK_OK
LABEL_MEM_CHK_FAIL:
	mov	dword [_dwMCRNumber], 0
LABEL_MEM_CHK_OK:

	; 初始化全局描述符
	InitDescBase LABEL_SEG_CODE16,LABEL_DESC_CODE16
	InitDescBase LABEL_SEG_CODE32,LABEL_DESC_CODE32
	InitDescBase LABEL_DATA, LABEL_DESC_DATA
	InitDescBase LABEL_STACK, LABEL_DESC_STACK
	; 初始化任务描述符0
	InitTaskDescBase 0
	; 初始化任务描述符1
	InitTaskDescBase 1
	; 初始化任务描述符2
	InitTaskDescBase 2
	; 初始化任务描述符3
	InitTaskDescBase 3
	; 为加载 GDTR 作准备
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_GDT		; eax <- gdt 基地址
	mov	dword [GdtPtr + 2], eax	; [GdtPtr + 2] <- gdt 基地址
	; 为加载 IDTR 作准备
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_IDT		; eax <- idt 基地址
	mov	dword [IdtPtr + 2], eax	; [IdtPtr + 2] <- idt 基地址
	; 保存 IDTR
	sidt	[_SavedIDTR]
	; 保存中断屏蔽寄存器(IMREG)值
	in	al, 21h
	mov	[_SavedIMREG], al
	; 加载 GDTR
	lgdt	[GdtPtr]
	; 关中断
	cli
	; 加载 IDTR
	lidt	[IdtPtr]
	; 打开地址线A20
	in	al, 92h
	or	al, 00000010b
	out	92h, al
	; 准备切换到保护模式
	mov	eax, cr0
	or	eax, 1
	mov	cr0, eax
	; 真正进入保护模式
	jmp	dword SelectorCode32:0	; 执行这一句会把 SelectorCode32 装入 cs, 并跳转到 Code32Selector:0  处
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LABEL_REAL_ENTRY:		; 从保护模式跳回到实模式就到了这里
	mov	ax, cs
	mov	ds, ax
	mov	es, ax
	mov	ss, ax
	mov	sp, [_wSPValueInRealMode]; 将实模式下的栈指针 sp 设置为内存中的_wSPValueInRealMode变量的值

	lidt	[_SavedIDTR]	; 恢复 IDTR 的原值

	mov al, [_SavedIMREG]  ; 将内存中保存的中断屏蔽寄存器 IMREG 的值加载到 al 寄存器
    out 21h, al             ; 将 al 寄存器的值写入端口 0x21，恢复中断屏蔽寄存器的原始状态

	in al, 92h              ; 从端口 0x92 读取一个字节到 al 寄存器
    and al, 11111101b       ; 将 al 寄存器的值与掩码 0b11111101 进行按位与操作，关闭 A20 地址线
    out 92h, al             ; 将 al 寄存器的值写回端口 0x92，确保 A20 地址线关闭

    sti                      ; 开启中断，允许外部中断响应

    mov ax, 4c00h           ; 将 DOS 退出码 0x4c00 加载到 ax 寄存器
    int 21h                  ; 调用 DOS 中断 0x21，返回到 DOS 操作系统
; END of [SECTION .s16]



[SECTION .s32]; 32 位代码段. 由实模式跳入.
[BITS	32]
LABEL_SEG_CODE32:
	mov	ax, SelectorData
	mov	ds, ax			; 数据段选择子
	mov	es, ax
	mov	ax, SelectorVideo
	mov	gs, ax			; 视频段选择子
	mov	ax, SelectorStack
	mov	ss, ax			; 堆栈段选择子
	mov	esp, TopOfStack

	; 初始化8253A
	call	Init8253A
	call	Init8259A
	; 清屏
	call	ClearScreen
	; 下面显示一个字符串
	push	szPMMessage
	call	DispStr
	add	esp, 4
	push	szMemChkTitle
	call	DispStr
	add	esp, 4
	call	DispMemSize		; 显示内存信息
	; 计算页表个数
	xor	edx, edx
	mov	eax, [dwMemSize]
	mov	ebx, 400000h	; 400000h = 4M = 4096 * 1024, 一个页表对应的内存大小
	div	ebx
	mov	ecx, eax	; 此时 ecx 为页表的个数，也即 PDE 应该的个数
	test	edx, edx
	jz	.no_remainder
	inc	ecx		; 如果余数不为 0 就需增加一个页表
.no_remainder:
	mov	[PageTableNumber], ecx	; 暂存页表个数
	call	LABEL_INIT_PAGE_TABLE0
	call	LABEL_INIT_PAGE_TABLE1
	call	LABEL_INIT_PAGE_TABLE2
	call	LABEL_INIT_PAGE_TABLE3
	; 初始化ticks
	xor 	ecx, ecx 
.initTicks:
	mov     eax, dword [TaskPriority + ecx*4]             
	mov     dword [LeftTicks + ecx*4], eax
	inc   	ecx
	cmp    	ecx, 4
	jne     .initTicks
	xor 	ecx, ecx  
	sti							; 打开中断
	mov		eax, PageDirBase0	; ┳ 加载 CR3
	mov		cr3, eax			; ┛
	mov		ax, SelectorTSS0	; ┳ 加载 TSS
	ltr		ax					; ┛
	mov		eax, cr0			; ┓
	or		eax, 80000000h		; ┣ 打开分页
	mov		cr0, eax			; ┃
	jmp		short .1			; ┛
.1:
	nop
	; 提示初始化完成
.ready:
	xor 	ecx, ecx
	mov		ah, 0Fh
.outputLoop:
	mov		al, [szReadyMessage + ecx]
	mov 	[gs:((80 * 19 + ecx) * 2)], ax
	inc		ecx
	cmp		al, 0
	jnz		.outputLoop
	SwitchTask 0
	call	SetRealmode8259A	; 恢复 8259A 以顺利返回实模式, 未执行
	jmp		SelectorCode16:0	; 返回实模式, 未执行

; int handler ------------------------------------------------------------------
_ClockHandler:
ClockHandler    equ _ClockHandler - $$    ; 定义中断处理程序的入口地址

    push ds        ; 保存 ds 寄存器的值
    pushad         ; 保存通用寄存器的值

    mov eax, SelectorData    ; 将 SelectorData 地址加载到 eax 寄存器
    mov ds, ax               ; 将 ds 寄存器设置为 SelectorData

    mov al, 0x20            ; 将 0x20 写入 al 寄存器
    out 0x20, al            ; 将 al 寄存器的值写入端口 0x20，向主片发送中断结束信号

    ; 检查当前运行的任务是否需要切换
    mov edx, dword [RunningTask]          ; 将当前运行任务的索引加载到 edx 寄存器
    mov ecx, dword [LeftTicks+edx*4]      ; 将当前任务的剩余时钟周期加载到 ecx 寄存器
    test ecx, ecx                         ; 测试 ecx 寄存器是否为零
    jnz .subTicks                         ; 如果 ecx 不为零，说明当前任务还未执行完毕，直接跳转到 .subTicks，不进行任务切换

    ; 如果当前任务已经执行完毕，检查所有任务是否已经全部执行完毕
    mov eax, dword [LeftTicks]             ; 加载第一个任务的剩余时钟周期到 eax 寄存器
    mov ebx, edx                           ; 备份当前运行任务的索引到 ebx 寄存器
    or eax, dword [LeftTicks + 4]          ; 检查第二个任务的剩余时钟周期
    or eax, dword [LeftTicks + 8]          ; 检查第三个任务的剩余时钟周期
    or eax, dword [LeftTicks + 12]         ; 检查第四个任务的剩余时钟周期
    jz .allFinished                        ; 如果所有任务的剩余时钟周期都为零，则跳转到 .allFinished，重新赋值

.goToNext:  ; 选择下一个任务
    xor eax, eax                         ; 清空 eax 寄存器
    xor esi, esi                         ; 清空 esi 寄存器
    xor ecx, ecx                         ; 清空 ecx 寄存器

.getMaxLoop:  ; 获取Ticks最大的任务
    cmp dword [LeftTicks+eax*4], ecx    ; 比较当前任务的剩余时钟周期与最大时钟周期
    jle .notMax                         ; 如果当前任务的剩余时钟周期小于等于最大时钟周期，跳转到 .notMax
    mov ecx, dword [TaskPriority+eax*4]    ; 更新最大时钟周期为当前任务的时钟周期
    mov ebx, eax                             ; 更新最大时钟周期对应的任务索引
    mov esi, 1                               ; 设置标志位指示找到了更大的时钟周期

.notMax:  
    add eax, 1                       ; 更新当前任务索引
    cmp eax, 4                       ; 检查是否已经检查完所有任务
    jnz .getMaxLoop                  ; 如果还有任务未检查完，则继续循环获取最大时钟周期任务

    mov eax, esi                     ; 将标志位值（1 或 0）加载到 eax 寄存器
    test al, al                      ; 测试标志位是否为零
    jz .subTicks                     ; 如果标志位为零，说明当前没有更大的时钟周期任务，直接跳转到 .subTicks

    mov dword [RunningTask], ebx     ; 将最大时钟周期任务的索引存入 RunningTask
    mov edx, ebx                     ; 将最大时钟周期任务的索引加载到 edx 寄存器
    ; 跳转到对应任务的处理代码
    cmp edx, 0
    je .switchToTask0
    cmp edx, 1
    je .switchToTask1
    cmp edx, 2
    je .switchToTask2
    cmp edx, 3
    je .switchToTask3
    jmp .exit

.switchToTask0:
    SwitchTask 0    ; 切换到任务 0 的处理代码
.switchToTask1:
    SwitchTask 1    ; 切换到任务 1 的处理代码
.switchToTask2:
    SwitchTask 2    ; 切换到任务 2 的处理代码
.switchToTask3:
    SwitchTask 3    ; 切换到任务 3 的处理代码

.subTicks:  
    sub dword [LeftTicks+edx*4], 1    ; 将当前任务的剩余时钟周期减一
    jmp .exit                          ; 跳转到退出标签

; 如果所有任务都执行完毕，重新赋值
.allFinished:  ; Local function
    xor ecx, ecx    ; 清空 ecx 寄存器
.setLoop:
    mov eax, dword [TaskPriority + ecx*4]    ; 将任务优先级加载到 eax 寄存器
    mov dword [LeftTicks + ecx*4], eax       ; 重新赋值每个任务的剩余时钟周期
    inc ecx                                   ; 更新循环计数器
    cmp ecx, 4                                ; 检查是否已经处理完所有任务
    jne .setLoop                              ; 如果还有任务未处理完，则继续循环

    xor ecx, ecx    ; 清空 ecx 寄存器
    jmp .goToNext   ; 跳转到选择下一个任务的标签
                        
.exit:
    popad         ; 恢复通用寄存器的值
    pop ds        ; 恢复 ds 寄存器的值
    iretd         ; 中断返回指令


; ---------------------------------------------------------------------------
_UserIntHandler:
UserIntHandler	equ	_UserIntHandler - $$
	mov	ah, 0Ch				; 0000: 黑底    1100: 红字
	mov	al, 'I'
	mov	[gs:((80 * 0 + 70) * 2)], ax	; 屏幕第 0 行, 第 70 列。
	iretd

_SpuriousHandler:
SpuriousHandler	equ	_SpuriousHandler - $$
	mov	ah, 0Ch				; 0000: 黑底    1100: 红字
	mov	al, '!'
	mov	[gs:((80 * 0 + 75) * 2)], ax	; 屏幕第 0 行, 第 75 列。
	iretd
; ---------------------------------------------------------------------------

InitPageTable 0
InitPageTable 1
InitPageTable 2
InitPageTable 3

%include	"lib.inc"	; 库函数
SegCode32Len	equ	$ - LABEL_SEG_CODE32
; END of [SECTION .s32]


; 16 位代码段, 由 32 位代码段跳入, 跳出后到实模式.
[SECTION .s16code]
ALIGN	32
[BITS	16]
LABEL_SEG_CODE16:
	; 跳回实模式:
	mov		ax, SelectorNormal
	mov		ds, ax
	mov		es, ax
	mov		fs, ax
	mov		gs, ax
	mov		ss, ax

	mov		eax, cr0
	; and		al, 11111110b	; 仅切换到实模式
	and		eax, 7ffffffeh		; 切换到实模式并关闭分页
	mov		cr0, eax

LABEL_GO_BACK_TO_REAL:
	jmp		0:LABEL_REAL_ENTRY	; 段地址会在程序开始处被设置成正确的值

Code16Len	equ	$ - LABEL_SEG_CODE16
; END of [SECTION .s16code]
