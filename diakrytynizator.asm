global _start
    SYS_READ equ 0
    SYS_WRITE equ 1
    SYS_EXIT equ 60

    STDOUT equ 1
    STDIN equ 0

    BUFSIZE equ 4000

    MODULUS equ 0x10FF80

%macro mov8 2 ; mov qword via a layover at rax
    mov rax, %2
    mov %1, rax
%endmacro

%macro add8 2 ; add qwords via a layover at rax
    mov rax, %2
    add %1, rax
%endmacro

%macro sub8 2 ; sub qwords via a layover at rax
    mov rax, %2
    sub %1, rax
%endmacro

%macro mult8 2 ; mult8 a,b means a<-a*b
    mov rax, %1
    xor rdx, rdx
    mult8 %2
    mov %1, rax
%endmacro

%macro mod8 2 ; mod8 a,b means a:=(a mod8 b)
    mov rdx, 0
    mov rax, %1
    mov rcx, %2
    div rcx
    mov %1, rdx
%endmacro

%macro multmod8 3 ; mod8 a,b,c means a:=(a*b mod8 c)
    mov rax, %2
    mul %1
    mov rbx, %3
    div rbx
    mov %1, rdx
%endmacro

%macro print 2 ; print size whatToPrint
   mov rax, SYS_WRITE
   mov rdi, STDOUT
   mov rsi, %2
   mov rdx, %1
   syscall
%endmacro

%macro pdep_qword 3;
    mov rbx, %2
    mov rcx, %3
    pdep rax, rbx, rcx
    mov %1, rax
%endmacro

section .bss

argc: resq 1
deg: resq 1
coeffsLeft: resq 1
rspBackup: resq 1

buforIn: resb BUFSIZE
buforInWsk: resb BUFSIZE
buforInLeft: resq 1

wynikIn: resq 1
ileOdczytano: resq 1

currentMonomial: resq 1
coeffBufor: resq 1
wynikWielomianu: resq 1

wynikOut: resq 1
ileZapisano: resq 1
wskOut: resq 1

exitCode: resq 1

section .text
_start:
parseArguments:
    pop qword [argc]
    pop rbx ; ignore filename - cleaner than: add/sub rsp, 8
    mov [rspBackup], rsp
.readNextCoeff:
    cmp qword [argc], 1
    jle .finishedParsing
    sub qword [argc], 1
    add qword [deg], 1
    mov qword [coeffBufor], 0
    pop rbx
.readCoeffByte:
    mov r10, [rbx]
    and r10, 0xFF
    cmp r10b, 0
    je .finishCoeff
    sub r10b, '0'
    mov rax, 10
    mul qword [coeffBufor]
    mov qword [coeffBufor], rax
    add qword [coeffBufor], r10
    add rbx, 1
    jmp .readCoeffByte
.finishCoeff:
    push qword [coeffBufor]
    add rsp, 8
    jmp .readNextCoeff
.finishedParsing:
    mov rsp, [rspBackup] ; stack has the same size as before parsing - move pointer so noone will overwrite the coeffs
.readCalcWriteLoop:
    call .readUnicode
    mov rsp, [rspBackup]
    mov8 [wynikWielomianu], [wynikIn]
    cmp qword [wynikIn], 0x80
    jl .write
.calc:
    sub qword [wynikIn], 0x80
    mov qword [wynikWielomianu], 0
    mov8 [coeffsLeft], [deg]
    mov qword [currentMonomial], 1 ; x^0
.calcNext:
    cmp qword [coeffsLeft], 0
    jle .calcReturn
    pop qword [coeffBufor]

    multmod8 qword [coeffBufor], qword [currentMonomial], MODULUS
    add8 [wynikWielomianu], [coeffBufor]
    mod8 [wynikWielomianu], MODULUS

    multmod8 qword [currentMonomial], qword [wynikIn], MODULUS

    sub qword [coeffsLeft], 1
    jmp .calcNext
.calcReturn:
    mov rsp, [rspBackup]
    add qword [wynikWielomianu], 0x80
.write:
    call .writeUnicode

    mov rax, [wynikOut]
    bswap rax
    mov [wynikOut], rax

    mov8 [wskOut], wynikOut
    add8 [wskOut], 8
    sub8 [wskOut], [ileZapisano]

    print [ileZapisano], [wskOut]
    ;print 8, wynikOut

    jmp .readCalcWriteLoop

.readByteToAl:
    call .refillBuforIfEmpty
    mov al, 0

    mov rbx, [buforInWsk]
    mov al, [rbx]
    add qword [buforInWsk], 1
    sub qword [buforInLeft], 1
    ret
.refillBuforIfEmpty:
    cmp qword [buforInLeft], 0
    jg .refillBuforReturn
    mov rax, SYS_READ
    mov rdi, STDIN
    mov rsi, buforIn
    mov rdx, BUFSIZE
    syscall
    cmp rax, 0
    mov qword [exitCode], 1
    jl .exit
    mov qword [exitCode], 0
    je .exit
    mov [buforInLeft], rax
    mov qword [buforInWsk], buforIn
.refillBuforReturn:
    ret

.readUnicode:
    call .readByteToAl
    mov r8, 0
    mov r8b, al
    cmp r8, 10000000b
    jl .readUtf1
    cmp r8, 11011111b
    jle .readUtf2
    cmp r8, 11101111b
    jle .readUtf3
    cmp r8, 11110111b
    jle .readUtf4
    jmp .readUtfError
.readUtf1: ; 7 bits
    mov [wynikIn],r8
    mov byte [ileOdczytano], 1
    ret
.readUtf2: ; 13 bits
    and r8, 00011111b
    mov [wynikIn], r8
    call .readUnicodeTailByte
    mov byte [ileOdczytano], 2

    cmp qword [wynikIn], 0x80 ; wasteful unicode usage
    jl .readUtfError

    ret
.readUtf3: ; 19 bits
    and r8, 00001111b
    mov [wynikIn], r8
    call .readUnicodeTailByte
    call .readUnicodeTailByte
    mov byte [ileOdczytano], 3

    cmp qword [wynikIn], 0x800 ; wasteful unicode usage
    jl .readUtfError

    ret
.readUtf4: ; 25 bits, but max 0x10FFFF
    and r8, 00000111b
    mov [wynikIn], r8
    call .readUnicodeTailByte
    call .readUnicodeTailByte
    call .readUnicodeTailByte

    cmp qword [wynikIn], 0x10000 ; wasteful unicode usage
    jl .readUtfError

    cmp qword [wynikIn], 0x10FFFF ; maximal unicode value
    jg .readUtfError
    mov byte [ileOdczytano], 4
    ret
.readUtfError:
    mov qword [exitCode], 1
    jmp .exit
    ret

.readUnicodeTailByte:
    shl dword [wynikIn], 6
    call .readByteToAl
    mov r8, 0
    mov r8b, al
    and r8, 00111111b
    add [wynikIn], r8
    ret

.writeUnicode:
    mov8 [wynikOut], 0
    mov r8, [wynikWielomianu]
    cmp r8, 0x7F
    jle .writeUtf1
    cmp r8, 0x80
    jl .writeUtfError
    cmp r8, 0x7FF
    jle .writeUtf2
    cmp r8, 0x800
    jl .writeUtfError
    cmp r8, 0xFFFF
    jle .writeUtf3
    cmp r8, 0x10000
    jl .writeUtfError
    cmp r8, 0x10FFFFF
    jle .writeUtf4
    jmp .writeUtfError
.writeUtf1:
    mov8 [wynikOut], [wynikWielomianu]
    mov byte [ileZapisano],1
    ret
.writeUtf2:
    pdep_qword [wynikOut], [wynikWielomianu], 0001111100111111b ; 00011111 00111111
    add qword [wynikOut],                     1100000010000000b ; 11000000 10000000
    mov byte [ileZapisano],2
    ret
.writeUtf3:
    pdep_qword [wynikOut], [wynikWielomianu], 000011110011111100111111b ; 00001111 00111111 00111111
    add qword [wynikOut],                     111000001000000010000000b ; 11100000 10000000 10000000
    mov byte [ileZapisano],3
    ret
.writeUtf4:
    pdep_qword [wynikOut], [wynikWielomianu], 00000111001111110011111100111111b ; 00000111 00111111 00111111 00111111
    add8 [wynikOut],                          11110000100000001000000010000000b ; 11110000 10000000 10000000 10000000
    mov byte [ileZapisano],4
    ret
.writeUtfError:
    mov qword [exitCode], 1
    jmp .exit
    ret
.exit:
    mov rax, SYS_EXIT
    mov rdi, [exitCode]
    syscall

    xor rax, rax
    ret