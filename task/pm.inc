; 定义常量和宏

DA_32           EQU 4000h    ; 定义常量 DA_32，表示32位段
DA_LIMIT_4K     EQU 8000h    ; 定义常量 DA_LIMIT_4K，表示段界限粒度为4K字节

DA_DPL0         EQU 00h      ; 定义常量 DA_DPL0，表示特权级为0
DA_DPL1         EQU 20h      ; 定义常量 DA_DPL1，表示特权级为1
DA_DPL2         EQU 40h      ; 定义常量 DA_DPL2，表示特权级为2
DA_DPL3         EQU 60h      ; 定义常量 DA_DPL3，表示特权级为3


; 存储段描述符类型值说明

DA_DR		EQU	90h	; 存在的只读数据段类型值
DA_DRW		EQU	92h	; 存在的可读写数据段属性值
DA_DRWA		EQU	93h	; 存在的已访问可读写数据段类型值
DA_C		EQU	98h	; 存在的只执行代码段属性值
DA_CR		EQU	9Ah	; 存在的可执行可读代码段属性值
DA_CCO		EQU	9Ch	; 存在的只执行一致代码段属性值
DA_CCOR		EQU	9Eh	; 存在的可执行可读一致代码段属性值


; 系统段描述符类型值说明

DA_LDT		EQU	  82h	; 局部描述符表段类型值
DA_TaskGate	EQU	  85h	; 任务门类型值
DA_386TSS	EQU	  89h	; 可用 386 任务状态段类型值
DA_386CGate	EQU	  8Ch	; 386 调用门类型值
DA_386IGate	EQU	  8Eh	; 386 中断门类型值
DA_386TGate	EQU	  8Fh	; 386 陷阱门类型值


; 选择子类型值说明
; 其中:
;       SA_  : Selector Attribute

SA_RPL0     EQU 0    ; 特权级别（RPL）为0
SA_RPL1     EQU 1    ; 特权级别（RPL）为1
SA_RPL2     EQU 2    ; 特权级别（RPL）为2
SA_RPL3     EQU 3    ; 特权级别（RPL）为3

SA_TIG      EQU 0    ; 描述符类型位（TI）为全局描述符表
SA_TIL      EQU 4    ; 描述符类型位（TI）为局部描述符表



; 分页机制使用的常量说明

PG_P		EQU	1	; 页存在属性位
PG_RWR		EQU	0	; R/W 属性位值, 读/执行
PG_RWW		EQU	2	; R/W 属性位值, 读/写/执行
PG_USS		EQU	0	; U/S 属性位值, 系统级
PG_USU		EQU	4	; U/S 属性位值, 用户级



; 宏 
;
; 描述符
; usage: Descriptor Base, Limit, Attr
;        Base:  dd
;        Limit: dd (low 20 bits available)
;        Attr:  dw (lower 4 bits of higher byte are always 0)
%macro Descriptor 3
	dw	%2 & 0FFFFh				; 段界限 1				(2 字节)
	dw	%1 & 0FFFFh				; 段基址 1				(2 字节)
	db	(%1 >> 16) & 0FFh			; 段基址 2				(1 字节)
	dw	((%2 >> 8) & 0F00h) | (%3 & 0F0FFh)	; 属性 1 + 段界限 2 + 属性 2		(2 字节)
	db	(%1 >> 24) & 0FFh			; 段基址 3				(1 字节)
%endmacro ; 共 8 字节
;
; 门
; usage: Gate Selector, Offset, DCount, Attr
;        Selector:  dw
;        Offset:    dd
;        DCount:    db
;        Attr:      db
%macro Gate 4
	dw	(%2 & 0FFFFh)				; 偏移 1				(2 字节)
	dw	%1					; 选择子				(2 字节)
	dw	(%3 & 1Fh) | ((%4 << 8) & 0FF00h)	; 属性					(2 字节)
	dw	((%2 >> 16) & 0FFFFh)			; 偏移 2				(2 字节)
%endmacro ; 共 8 字节
; 


; 初始化段描述符
; usage: InitDescBase LABEL, LABEL_DESC
; 宏：InitDescBase
; 用于初始化段描述符
; 参数1：LABEL - 段描述符标签
; 参数2：LABEL_DESC - 目标描述符标签
%macro InitDescBase 2
    xor eax, eax                        ; 清空 eax 寄存器
    mov ax, cs                          ; 将代码段寄存器 cs 的值加载到 ax 寄存器
    shl eax, 4                          ; 将 ax 寄存器的值左移4位，相当于乘以16，这是由于段描述符的地址格式
    add eax, %1                         ; 加上偏移量，得到段描述符的地址
    mov word [%2 + 2], ax               ; 将段描述符的段基址的低16位存入目标描述符标签的偏移地址+2处
    shr eax, 16                         ; 将 ax 寄存器的值右移16位，获得高16位
    mov byte [%2 + 4], al               ; 将段描述符的段基址的高8位存入目标描述符标签的偏移地址+4处
    mov byte [%2 + 7], ah               ; 将段描述符的段基址的高8位存入目标描述符标签的偏移地址+7处
%endmacro

; 初始化任务描述符
; usage: InitTaskDescBase num
; 宏：InitTaskDescBase
; 用于初始化任务描述符
; 参数1：num - 任务编号
%macro InitTaskDescBase 1
    InitDescBase LABEL_TASK%1_LDT, LABEL_TASK%1_DESC_LDT          ; 初始化任务的LDT描述符
    InitDescBase LABEL_TASK%1_CODE, LABEL_TASK%1_DESC_CODE        ; 初始化任务的代码段描述符
    InitDescBase LABEL_TASK%1_DATA, LABEL_TASK%1_DESC_DATA        ; 初始化任务的数据段描述符
    InitDescBase LABEL_TASK%1_STACK0, LABEL_TASK%1_DESC_STACK0    ; 初始化任务的堆栈0描述符
    InitDescBase LABEL_TASK%1_STACK3, LABEL_TASK%1_DESC_STACK3    ; 初始化任务的堆栈3描述符
    InitDescBase LABEL_TSS%1, LABEL_DESC_TSS%1                    ; 初始化任务的TSS描述符
%endmacro



; 定义LDT
; usage: DefineLDT num
; 用于定义LDT（本地描述符表）
; 参数1：num - 任务编号
%macro DefineLDT 1
[SECTION .ldt%1]; 定义LDT所在的段
ALIGN	32; 对齐到32字节边界
LABEL_TASK%1_LDT:; 任务%1的LDT标签
;                                         段基址       段界限     ,   属性
LABEL_TASK%1_DESC_DATA: Descriptor	0,	Task%1DataLen - 1,	 DA_DRWA + DA_DPL3; 数据段描述符
LABEL_TASK%1_DESC_CODE: Descriptor	0,	Task%1CodeLen - 1,	 DA_C + DA_32 + DA_DPL3; 代码段描述符
LABEL_TASK%1_DESC_STACK0: Descriptor	0,	TopOfTask%1Stack0,	 DA_DRWA + DA_32; 堆栈0描述符
LABEL_TASK%1_DESC_STACK3: Descriptor	0,	TopOfTask%1Stack3,	 DA_DRWA + DA_32 + DA_DPL3; 堆栈3描述符


TASK%1LDTLen		equ	$ - LABEL_TASK%1_LDT; 计算LDT长度
; LDT 选择子
SelectorTask%1Data equ LABEL_TASK%1_DESC_DATA - LABEL_TASK%1_LDT + SA_TIL + SA_RPL3     ; 数据段选择子
SelectorTask%1Code equ LABEL_TASK%1_DESC_CODE - LABEL_TASK%1_LDT + SA_TIL + SA_RPL3     ; 代码段选择子
SelectorTask%1Stack0 equ LABEL_TASK%1_DESC_STACK0 - LABEL_TASK%1_LDT + SA_TIL             ; 堆栈0选择子
SelectorTask%1Stack3 equ LABEL_TASK%1_DESC_STACK3 - LABEL_TASK%1_LDT + SA_TIL + SA_RPL3 ; 堆栈3选择子
; END of [SECTION .ldt]
%endmacro


;定义任务数据段
; usage: DefineTaskData num, string
; 宏：DefineTaskData
; 用于定义任务的数据段
; 参数1：num - 任务编号
; 参数2：string - 字符串
%macro DefineTaskData 2
[SECTION .task%1data]
ALIGN	32
[BITS	32]
LABEL_TASK%1_DATA:
	; 任务数据段
	_szTask%1Message: db %2, 0
	szTask%1Message	equ	_szTask%1Message - $$
Task%1DataLen	equ	$ - LABEL_TASK%1_DATA; 计算任务%1的数据段长度
; END of [SECTION .task%1data]
%endmacro

; 定义任务代码段
; usage: DefineTaskCode num, row, color
; 宏：DefineTaskCode
; 用于定义任务的代码段
; 参数1：num - 任务编号
; 参数2：row - 行号
; 参数3：color - 颜色
%macro DefineTaskCode 3
[SECTION .task%1code]
ALIGN	32
[BITS	32]
LABEL_TASK%1_CODE:
; 遍历输出szTask0Message
	xor 	ecx, ecx
	mov 	ah, %3 ; 将颜色值加载到 ah 寄存器
.outputLoop:
	mov al, [szTask%1Message + ecx]   ; 将任务消息字符串中的字符加载到 al 寄存器
    mov [gs:((80 * %2 + ecx) * 2)], ax ; 将字符和颜色值输出到屏幕指定位置
    inc ecx                            ; ecx 自增，指向下一个字符
    cmp al, 0                          ; 判断是否到达字符串末尾
    jne .outputLoop                    ; 如果未到达末尾则继续循环输出
    jmp LABEL_TASK%1_CODE             ; 跳转回任务%1的代码段标签，循环输出字符串

Task%1CodeLen	equ	$ - LABEL_TASK%1_CODE; 计算任务%1的代码段长度
; END of [SECTION .task%1code]
%endmacro

; 定义任务堆栈0
; usage: DefineTaskStack0 num
; 宏：DefineTaskStack0
; 用于定义任务的堆栈0
; 参数1：num - 任务编号
%macro DefineTaskStack0 1
[SECTION .task%1stack0]; 定义任务%1的堆栈0所在的段
ALIGN	32
[BITS	32]
LABEL_TASK%1_STACK0:
	times 512 db 0
TopOfTask%1Stack0	equ	$ - LABEL_TASK%1_STACK0 - 1
; END of [SECTION .task%1stack0]
%endmacro

; 定义任务堆栈3
; usage: DefineTaskStack3 num
; 宏：DefineTaskStack3
; 用于定义任务的堆栈3
; 参数1：num - 任务编号
%macro DefineTaskStack3 1
[SECTION .task%1stack3]; 定义任务%1的堆栈3所在的段
ALIGN	32
[BITS	32]
LABEL_TASK%1_STACK3:
	times 512 db 0
TopOfTask%1Stack3	equ	$ - LABEL_TASK%1_STACK3 - 1
; END of [SECTION .task%1stack3]
%endmacro

; 定义任务TSS

; 宏：DefineTaskTSS
; 用于定义任务的TSS（任务状态段）
; 参数1：num - 任务编号
%macro DefineTaskTSS 1
[SECTION .tss%1]
ALIGN	32
[BITS	32]
LABEL_TSS%1:
		DD	0			; Back
		DD	TopOfStack		; 0 级堆栈
		DD	SelectorStack		; 
		DD	0			; 1 级堆栈
		DD	0			; 
		DD	0			; 2 级堆栈
		DD	0			; 
		DD	0			; CR3
		DD	0			; EIP
		DD	0			; EFLAGS
		DD	0			; EAX
		DD	0			; ECX
		DD	0			; EDX
		DD	0			; EBX
		DD	0			; ESP
		DD	0			; EBP
		DD	0			; ESI
		DD	0			; EDI
		DD	0			; ES
		DD	0			; CS
		DD	0			; SS
		DD	0			; DS
		DD	0			; FS
		DD	0			; GS
		DD	0			; LDT
		DW	0			; 调试陷阱标志
		DW	$ - LABEL_TSS%1 + 2	; I/O位图基址
		DB	0ffh			; I/O位图结束标志
TSS%1Len		equ	$ - LABEL_TSS%1 ; 计算任务%1的TSS长度
%endmacro
; TSS 



; 定义任务
; usage: DefineTask num, string, row,color
; 宏：DefineTask
; 用于定义任务及其相关描述符
; 参数1：num - 任务编号
; 参数2：string - 任务消息字符串
; 参数3：row - 行号
; 参数4：color - 颜色
%macro DefineTask 4
	DefineLDT	%1
	DefineTaskCode	%1, %3, %4
	DefineTaskData	%1, %2
	DefineTaskStack0	%1
	DefineTaskStack3	%1
	DefineTaskTSS	%1
%endmacro

; 定义初始化页表

; 宏：InitPageTable
; 用于初始化页表
; 参数1：num - 页表编号
%macro InitPageTable 1
LABEL_INIT_PAGE_TABLE%1:
		; 初始化页目录
	mov		ax, SelectorFlatRW
	mov		es, ax
	mov		edi, PageDirBase%1		; 此段首地址为 PageDirBase1
	xor		eax, eax
	mov		eax, PageTblBase%1 | PG_P  | PG_USU | PG_RWW; 设置页表属性
	mov		ecx, [PageTableNumber]
.1:
	stosd
	add		eax, 4096				; 为了简化, 所有页表在内存中是连续的.
	loop	.1
	; 再初始化所有页表
	mov		eax, [PageTableNumber]	; 将页表个数加载到 eax 寄存器
	mov		ebx, 1024				; 每个页表 1024 个 PTE
	mul		ebx
	mov		ecx, eax				; PTE个数 = 页表个数 * 1024
	mov		edi, PageTblBase%1		; 将页表基址加载到 edi 寄存器
	xor		eax, eax
	mov		eax, PG_P  | PG_USU | PG_RWW; 设置页表属性
.2:
	stosd
	add		eax, 4096				; 每一页指向 4K 的空间
	loop	.2

	ret
; End of SetupPaging1 
%endmacro


; 切换到任务
; 宏：SwitchTask
; 用于切换到指定任务
; 参数1：num - 任务编号
%macro SwitchTask 1
    mov ax, SelectorLDT%1       ; 将任务%1的LDT选择子加载到 ax 寄存器
    lldt ax                     ; 使用 lldt 指令加载 LDT
    mov eax, PageDirBase%1      ; 将任务%1的页目录基址加载到 eax 寄存器
    mov cr3, eax                ; 将 eax 寄存器的值加载到 CR3 控制寄存器
    mov eax, SelectorTask%1Data ; 将任务%1的数据段选择子加载到 eax 寄存器
    mov ds, eax                 ; 将 eax 寄存器的值加载到 ds 寄存器

    ; 使用 iretd 进行任务切换，切换至任务%1
    push SelectorTask%1Stack3   ; 压入堆栈3的选择子
    push TopOfTask%1Stack3      ; 压入堆栈3的顶部地址
    pushfd                      ; 压入标志寄存器内容
    pop eax                     ; 弹出标志寄存器内容到 eax
    or eax, 0x200               ; 将 IF 位置 1，开启中断
    push eax                    ; 将修改后的标志寄存器内容压入堆栈
    push SelectorTask%1Code     ; 压入任务%1的代码段选择子
    push 0                      ; 压入 EIP（指令指针）的偏移量
    iretd                       ; 执行 iretd 指令，实现任务切换
%endmacro
