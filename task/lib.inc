; Init8259A 
Init8259A:
	 mov al, 011h            ; ICW1: 初始化主片，工作在边沿触发模式，并启用级联
    out 020h, al            ; 将ICW1写入端口020h，设置主片的初始化控制字
    call io_delay           ; 调用延迟函数

    out 0A0h, al            ; ICW1: 初始化从片，工作在边沿触发模式，并启用级联
    call io_delay           ; 调用延迟函数

    mov al, 020h            ; ICW2: 设置主片的中断向量偏移为0x20，即IRQ0的中断向量号
    out 021h, al            ; 将ICW2写入端口021h，设置主片的中断向量基地址
    call io_delay           ; 调用延迟函数

    mov al, 028h            ; ICW2: 设置从片的中断向量偏移为0x28，即IRQ8的中断向量号
    out 0A1h, al            ; 将ICW2写入端口0A1h，设置从片的中断向量基地址
    call io_delay           ; 调用延迟函数

    mov al, 004h            ; ICW3: 设置主片的IR2引脚与从片相连
    out 021h, al            ; 将ICW3写入端口021h，设置主片的IR2引脚连接从片
    call io_delay           ; 调用延迟函数

    mov al, 002h            ; ICW3: 设置从片的从片号为2
    out 0A1h, al            ; 将ICW3写入端口0A1h，设置从片的从片号
    call io_delay           ; 调用延迟函数

    mov al, 001h            ; ICW4: 设置主片和从片的工作模式
    out 021h, al            ; 将ICW4写入端口021h，设置主片的工作模式
    call io_delay           ; 调用延迟函数

    out 0A1h, al            ; 将ICW4写入端口0A1h，设置从片的工作模式
    call io_delay           ; 调用延迟函数

    mov al, 11111110b       ; OCW1: 仅开启定时器中断，关闭其余所有中断
    out 021h, al            ; 将OCW1写入端口021h，设置主片的OCW1
    call io_delay           ; 调用延迟函数

    mov al, 11111111b       ; OCW1: 屏蔽从片的所有中断
    out 0A1h, al            ; 将OCW1写入端口0A1h，设置从片的OCW1
    call io_delay           ; 调用延迟函数

    ret                      ; 返回
; END of Init8259A 

; SetRealmode8259A 
; 设置实模式下的8259A中断控制器
SetRealmode8259A:
    mov ax, SelectorData       ; 将数据段选择子加载到 ax 寄存器
    mov fs, ax                 ; 将 ax 寄存器的值加载到 fs 寄存器

    mov al, 017h               ; ICW1: 设置主片的工作模式为边沿触发
    out 020h, al               ; 将ICW1写入端口020h，设置主片的工作模式
    call io_delay              ; 调用延迟函数

    mov al, 008h               ; ICW2: 设置主片的中断向量偏移为0x8，即IRQ0的中断向量号
    out 021h, al               ; 将ICW2写入端口021h，设置主片的中断向量基地址
    call io_delay              ; 调用延迟函数

    mov al, 001h               ; ICW4: 设置主片的工作模式
    out 021h, al               ; 将ICW4写入端口021h，设置主片的工作模式
    call io_delay              ; 调用延迟函数

    mov al, [fs:SavedIMREG]   ; 恢复中断屏蔽寄存器 IMREG 的原值
    out 021h, al               ; 将IMREG的原值写入端口021h
    call io_delay              ; 调用延迟函数

    ret                         ; 返回
; END of SetRealmode8259A 

; Init8253A 
; 初始化 8253A 定时器
Init8253A:
	mov		al, 00110110b		; 通道 0 的 CONTROL 字节
	out		043h, al			; 设置 8253A 芯片, 2 字节计数值, 模式 3, 二进制计数
	call	io_delay

	mov		ax, 59659			; 频率 20 Hz, 时钟周期为50ms, 设置 COUNT 为 1193180 / 20 = 59659
	out		040h, al			; 将 COUNT 的低位写入通道 0
	call	io_delay

	mov		al, ah
	out		040h, al			; 将 COUNT 的高位写入通道 0
	call	io_delay

	ret
; END of Init8253A 

; io_delay 
; 等待函数，通过一系列nop指令产生延迟
io_delay:
	nop
	nop
	nop
	nop
	ret
; END of io_delay 


; 显示 AL 中的数字

DispAL:
	push	ecx
	push	edx
	push	edi

	mov	edi, [dwDispPos]; 将显示位置加载到 edi 寄存器

	mov     ah, 0Fh            ; 设置字符属性，黑底白字
    mov     dl, al             ; 将 al 寄存器的值加载到 dl 寄存器
    shr     al, 4              ; 取出 al 中的高4位，存入 al 中
    mov     ecx, 2             ; 设置循环次数为 2
.begin:
    and     al, 01111b         ; 将 al 寄存器的值与 01111b 位与，结果存入 al 中
    cmp     al, 9              ; 比较 al 的值与 9
    ja      .1                  ; 如果 al 大于 9，则跳转到标签 .1
    add     al, '0'            ; 如果 al 小于等于 9，则加上 '0'
    jmp     .2                  ; 跳转到标签 .2
.1:
    sub     al, 0Ah            ; 如果 al 大于 9，则减去 10
    add     al, 'A'            ; 加上 'A'
.2:
    mov     [gs:edi], ax       ; 将 ax 寄存器的值存入 edi 指向的内存地址
    add     edi, 2             ; edi 加上 2，指向下一个位置

    mov     al, dl             ; 将 dl 寄存器的值加载到 al 寄存器
    loop    .begin              ; 循环

    mov     [dwDispPos], edi   ; 更新显示位置

    pop     edi
    pop     edx
    pop     ecx

    ret
; DispAL 结束



; 显示一个整形数

DispInt:
 mov     eax, [esp + 4]      ; 将参数加载到 eax 寄存器
    shr     eax, 24             ; 将 eax 寄存器的值右移24位
    call    DispAL              ; 调用 DispAL 函数显示高位字节

    mov     eax, [esp + 4]      ; 将参数加载到 eax 寄存器
    shr     eax, 16             ; 将 eax 寄存器的值右移16位
    call    DispAL              ; 调用 DispAL 函数显示次高位字节

    mov     eax, [esp + 4]      ; 将参数加载到 eax 寄存器
    shr     eax, 8              ; 将 eax 寄存器的值右移8位
    call    DispAL              ; 调用 DispAL 函数显示次低位字节

    mov     eax, [esp + 4]      ; 将参数加载到 eax 寄存器
    call    DispAL              ; 调用 DispAL 函数显示低位字节

    mov     ah, 07h              ; 设置字符属性，黑底灰字
    mov     al, 'h'             ; 将 'h' 加载到 al 寄存器
    push    edi
    mov     edi, [dwDispPos]    ; 将显示位置加载到 edi 寄存器
    mov     [gs:edi], ax        ; 将 ax 寄存器的值存入 edi 指向的内存地址
    add     edi, 4              ; edi 加上 4，指向下一个位置
    mov     [dwDispPos], edi    ; 更新显示位置
    pop     edi

    ret                          ; 返回
; DispInt 结束


; 显示一个字符串

DispStr:
	push	ebp
	mov	ebp, esp
	push	ebx
	push	esi
	push	edi

	 mov     esi, [ebp + 8]      ; 将字符串地址加载到 esi 寄存器
    mov     edi, [dwDispPos]    ; 将显示位置加载到 edi 寄存器
    mov     ah, 0Fh             ; 设置字符属性，黑底白字
.1:
	lodsb
	test	al, al
	jz	.2
	cmp	al, 0Ah	; 是回车吗?
	jnz	.3
	push	eax
	mov	eax, edi
	mov	bl, 160
	div	bl
	and	eax, 0FFh
	inc	eax
	mov	bl, 160
	mul	bl
	mov	edi, eax
	pop	eax
	jmp	.1
.3:
	mov	[gs:edi], ax
	add	edi, 2
	jmp	.1

.2:
	 mov     [dwDispPos], edi    ; 更新显示位置

	pop	edi
	pop	esi
	pop	ebx
	pop	ebp
	ret
; DispStr 结束


; 换行

DispReturn:
    push    szReturn             ; 将换行符地址压入栈中
    call    DispStr              ; 调用 DispStr 函数显示换行符
    add     esp, 4               ; esp 加上 4
    ret                          ; 返回
; DispReturn 结束



; 内存拷贝， memcpy
; void* MemCpy(void* es:pDest, void* ds:pSrc, int iSize);

MemCpy:
	push	ebp
	mov	ebp, esp

	push	esi
	push	edi
	push	ecx

    mov     edi, [ebp + 8]       ; 将目的地址加载到 edi 寄存器
    mov     esi, [ebp + 12]      ; 将源地址加载到 esi 寄存器
    mov     ecx, [ebp + 16]      ; 将拷贝大小加载到 ecx 寄存器
.1:
    cmp     ecx, 0               ; 比较 ecx 寄存器的值是否为零
    jz      .2                    ; 如果为零，跳转到 .2

    mov     al, [ds:esi]         ; 将源地址处的字节加载到 al 寄存器
    inc     esi                   ; esi 加 1
    mov     byte [es:edi], al     ; 将 al 寄存器的值存入目的地址处
    inc     edi                   ; edi 加 1

    dec     ecx                   ; ecx 减 1
    jmp     .1                    ; 跳转到 .1
.2:
    mov     eax, [ebp + 8]       ; 将目的地址加载到 eax 寄存器

    pop     ecx
    pop     edi
    pop     esi
    mov     esp, ebp
    pop     ebp
    ret                           ; 返回
; MemCpy 结束 

; ClearScreen 清屏函数，将屏幕所有字符设置为黑色
ClearScreen:
	push	eax
	push	ebx
	push	ecx

    mov     ah, 00000000b        ; 设置字符属性，黑底黑字
    mov     al, 0                 ; 将 al 寄存器清零
    mov     ebx, 0                ; 将 ebx 寄存器清零
    mov     ecx, 4000             ; 设置循环次数为 4000/2 = 2000，即屏幕字符数的一半
.1:
    mov     [gs:ebx], ax         ; 将字符属性和字符数据存入屏幕内存地址
    add     ebx, 2                ; ebx 加上 2，指向下一个位置
    loop    .1                    ; 循环

    pop     ecx
    pop     ebx
    pop     eax

    ret                           ; 返回
; END of ClearScreen 

; DispMemSize 显示内存大小及分布信息
DispMemSize:
    push    esi
    push    edi
    push    ecx

    mov     esi, MemChkBuf       ; 将 MemChkBuf 地址加载到 esi 寄存器
    mov     ecx, [dwMCRNumber]   ; 将 MCRNumber 的值加载到 ecx 寄存器
.loop:
    mov     edx, 5                ; 设置循环次数为 5，每个ARDS结构有5个成员
    mov     edi, ARDStruct        ; 将 ARDStruct 地址加载到 edi 寄存器
.1:
    push    dword [esi]           ; 将 MemChkBuf[j * 4] 压栈
    call    DispInt               ; 调用 DispInt 函数显示一个成员
    pop     eax                    ; 弹出栈顶值，存入 eax 寄存器
    stosd                         ; 将 eax 寄存器的值存入 edi 指向的内存地址，然后 edi 加 4
    add     esi, 4                ; esi 加 4，指向下一个成员
    dec     edx                    ; edx 减 1
    cmp     edx, 0                ; 比较 edx 寄存器的值是否为零
    jnz     .1                     ; 如果不为零，跳转到 .1
    call    DispReturn            ; 调用 DispReturn 函数，显示换行符
    cmp     dword [dwType], 1     ; 比较 dwType 的值是否为 1
    jne     .2                     ; 如果不为 1，跳转到 .2
    mov     eax, [dwBaseAddrLow]  ; 将 dwBaseAddrLow 的值加载到 eax 寄存器
    add     eax, [dwLengthLow]    ; 将 dwLengthLow 的值加到 eax 寄存器
    cmp     eax, [dwMemSize]      ; 比较 eax 寄存器的值与 dwMemSize 的值
    jb      .2                     ; 如果小于 dwMemSize，跳转到 .2
    mov     [dwMemSize], eax      ; 将 eax 寄存器的值存入 dwMemSize
.2:
    loop    .loop                 ; 循环
    call    DispReturn            ; 调用 DispReturn 函数，显示换行符
    push    szRAMSize             ; 将 szRAMSize 地址压栈
    call    DispStr               ; 调用 DispStr 函数，显示字符串 "RAM size:"
    add     esp, 4                ; esp 加上 4
    push    dword [dwMemSize]     ; 将 dwMemSize 压栈
    call    DispInt               ; 调用 DispInt 函数，显示 dwMemSize 的值
    add     esp, 4                ; esp 加上 4

    pop     ecx
    pop     edi
    pop     esi
    ret                           ; 返回
; End of DispMemSize
