;Projecto AC - 4ª feira turno da tarde - Grupo 1: Rodrigo Correia n50230; Filipa Andrade n51394; Pedro Loureiro n51439

ORIG 0

;****************************
;*		 INTERRUPÇÕES		*
;****************************

interrupt0		WORD	Stop
interrupt1		WORD    RTClock


ORIG	20


;************************
;*		CONSTANTES		*
;************************
descerpiso1		EQU		0000000000000010b	;botão para descer do piso 1 carregado
descerpiso2		EQU		0000000000000100b	;botão para descer do piso 2 carregado
descerpiso3		EQU		0000000000001000b	;botão para descer do piso 3 carregado
subirpiso0		EQU		0000000000010000b	;botão para subir do piso 0 carregado
subirpiso1		EQU		0000000000100000b	;botão para subir do piso 1 carregado
subirpiso2		EQU		0000000001000000b	;botão para subir do piso 2 carregado
botao0			EQU		0000000100000000b	;botão 0 da cabine carregado
botao1			EQU		0000001000000000b	;botão 1 da cabine carregado
botao2			EQU		0000010000000000b	;botão 2 da cabine carregado
botao3			EQU		0000100000000000b	;botão 3 da cabine carregado
piso0			EQU		0001000000000000b	;elevador no piso 0
piso1			EQU		0010000000000000b	;elevador no piso 1
piso2			EQU		0100000000000000b	;elevador no piso 2
piso3			EQU		1000000000000000b	;elevador no piso 3
piso			EQU		1111000000000000b	;elevador num dos pisos

;constantes de escrita

fechaporta		EQU		1111111111111110b	;fecha a porta do elevador
abreporta		EQU		0000000000000001b	;abre a porta do elevador
motorsubir		EQU		0001000000000000b	;acciona motor para subir
motordescer		EQU		0010000000000000b	;acciona motor para descer
paramotor		EQU		0011000000000000b	;para motor do elevador
indicadorpiso0	EQU		0011111111111111b	;indicador de piso 0
indicadorpiso1	EQU		0100000000000000b	;indicador de piso 1
indicadorpiso2	EQU		1000000000000000b	;indicador de piso 2
indicadorpiso3	EQU		1100000000000000b	;indicador de piso 3

;estados do elevador

repouso			EQU		0	;elevador em repouso
movimento		EQU		1	;elevador em movimento
portaaberta		EQU		2	;elevador com a porta aberta, interrupção 1

;direcções do elevador

descer			EQU		0	;o elevador está ou vai descer
subir			EQU		1	;o elevador está ou vai subir
tantofaz		EQU		2	;o elevador não está a subir nem a descer


;********************
;*		WORDS		*
;********************
CONTROLO		WORD	1h				;variável que contém a informação sobre o controlo do elevador
ESTADO			WORD	portaaberta		;variável que guarda o estado actual do elevador e a direcção
DIRECCAO		WORD	tantofaz		;variável que guarda a direcção de movimento do elevador
PORTA			WORD	0h				;variável que passa a 1 quando passarem 3 segundos
OLDCONTROLO		WORD	0h
OLDESTADO		WORD	0h
OLDMOTOR		WORD	0h
OLDDIRECCAO		WORD	0h

;stop

STOPVALUE		WORD	1h

;****************************
;*		 INTERRUPÇÕES		*
;****************************
Stop:
	push	R0
	move	R0, 0h
	move	[STOPVALUE], R0
	pop		R0
	iret
	
				
RTClock:
	push	R0
	move	R0, 1h
	move	[PORTA], R0			  ;coloca a variável PORTA a 1
	move	R0, 0h
	move	[0fffch], R0		  ;coloca a 0 o RTClock
	pop		R0
	iret


;************************************
;*		INÍCIO E CICLO PRINCIPAL	*
;************************************
Inicio: 
	move	SP, 0fff0h				;inicialização do StackPointer
	call	Inicializacoes			;chama as inicializações dos portos e variáveis de saída
	
CicloPrincipal:
	call	BotoesElevador			;ciclo que actualiza os pedidos vindos dos botões da cabine do elevador
	;call	EscreveControlo
	call	BotoesPiso				;ciclo que actualiza os pedidos vindos dos botões de cada piso
	;call	EscreveControlo
	eni								;activar interrupções
	dsi
	call	actualiza_indicador_piso	;vai actualizar o indicador de piso através de uma função exterior 
	call	ProcessaPedidos			;ciclo que processa os pedidos efectuados
	call	EscreveControlo
	jmp		CicloPrincipal

	
	
;********************************************
;*	 INICIALIZAÇÃO DOS PORTOS DE CONTROLO	*
;********************************************
Inicializacoes:
	move	R2, 3000d				;inicialização a 3000 milisegundos
	move	[0fffch], R2			;do porto de controlo do RTC
	move	R3, 1500				;inicialização a 1500 milisegundos
	move	[0fffdh], R3			;do porto de controlo do tempo que o elevador demora a passar em cada piso (sensor activo)
	move	R4, 1500				;inicialização a 1500 milisegundos
	move	[0fffeh], R4			;do porto de controlo do tempo que o elevador demora entre pisos
	move	R0, 0000000000000001b	;inicialização com o bit 0 a 1, ou seja, com a porta aberta 
	move	[CONTROLO], R0			;do que se passa actualmente no elevador
	move	[0ffffh], R0			;do porto de controlo do elevador
	ret	



;************************************
;*		PEDIDOS BOTÕES ELEVADOR		*
;************************************
BotoesElevador:					;lê os botões do elevador pressionados e memoriza as luzes acesas - cria o byte de pedios c 8 bits
	move	R0, [0ffffh]		;coloca em R0 o endereço do porto de controlo do elevador (leitura)
	and		R0, 0f00h			;faz um and com 0f00h (0000111100000000b) para detectar que botões do elevador foram pressionados
	or		[CONTROLO], R0		
	ret							
	


;************************************
;*		PEDIDOS BOTÕES DE PISO		*
;************************************
BotoesPiso:
	move	R0, [0ffffh]		;coloca em R0 o endereço do porto de controlo do elevador (leitura)
	and		R0, 7Eh				;faz um and com 7eh (0000000001111110b) para detectar os pedidos (botões de cada piso pressionados)
	or		[CONTROLO], R0		;soma R0 com a variável de CONTROLO (actualiza o controlo)
	ret



;********************************
;*		PROCESSA PEDIDOS		*
;********************************
ProcessaPedidos:
	move	R0, [STOPVALUE]
	cmp		R0, 0h
	cz		RotinaStop
	call	EscreveControlo
	move	R0, [ESTADO]			;coloca em R0 o estado em que se encontra o elevador
	cmp		R0, repouso				;compara o estado actual do elevador com o estado repouso
	cz		RotinaRepouso			;se o elevador estiver em repouso chama a rotina RotinaRepouso
	call	EscreveControlo
	cmp		R0, movimento			;compara o estado actual do elevador com o estado movimento
	cz		RotinaMovimento			;se o elevador estiver em movimento chama a rotina RotinaMovimento
	call	EscreveControlo
	cmp		R0, portaaberta			;compara o estado actual do elevador com o estado portaaberta
	cz		RotinaPortaAberta		;se o elevador estiver a sair de um piso chama a rotina RotinaPortaAberta (int1)
	call	EscreveControlo
	ret
	
	
	
;********************************
;		ROTINAS DO ELEVADOR		*
;********************************
;************************
;*	  Rotina Repouso	*
;************************
RotinaRepouso:
	move	R0, [CONTROLO] 
	and		R0, 0f00h			;bits de sensores de botoes interiores
	shr		R0, 8				;temos em R0 os pedidos do elevador - dá prioridade a estes pedidos
	move	R1, [0ffffh]
	shr		R1, 12				;bits de sensores de piso actual - temos em R1 o piso actual
	move	R2, R0				
	move	R3, R0				;R0, R2, R3 - pedidos interiores
	move	R4, R1				;R1, R4 - piso actual
	cmp		R0, 0h
	jz		PedidosPiso				;se não houver pedidos no elevador vai ver se há pedidos nos botões de piso
	jmp		VerificaPedidosRepouso	;se houver pedidos no elevador vai verificar para que piso são: mesmo, inferior ou superior
	
	
	PedidosPiso:
	move	R0, [CONTROLO]
	and		R0, 0eh					;bits de sensores de botoes exteriores para descer
	move	R2, [CONTROLO]
	and		R2, 070h				;bits de sensores de botoes exteriores para subir
	shr		R2, 4
	or		R0, R2					;união de ambos os sets de botoes exteriores - temos em R0 os pedidos de piso
	move	R1, [0ffffh]
	shr		R1, 12					;bits de sensores de piso actual - temos em R1 o piso actual
	move	R2, R0				
	move	R3, R0					;R0,R2,R3 - pedidos de piso 
	;move	R4, R1					;R1,R4 - piso actual
	cmp		R0, 0h					;se n houver pedidos de piso vai para o fim
	jz		FimRotinaRepouso
	jmp		VerificaPedidosRepouso	;se houver pedidos vai verificar em que piso são: mesmo, inferior ou superior
	
			
	VerificaPedidosRepouso:				;verifica se há pedidos quer no elevador quer nos pisos
	
	;teste
;	PedidoPisoActualDireccao:
;		move	R5, [DIRECCAO]
;		cmp		R5, descer
;		jnz		PedidosPisoActualSubir
;		
;		PedidosPisoActualDescer:
;			move	R5, [CONTROLO]
;			and		R5, 0eh					;bits de sensores de botoes exteriores para descer
;			move	R4, R5
;			cmpl	R4
;			and		R5, R1
;			cnz		abre_porta
;			jmp		PedidoPisoActualRepouso
;		
;		PedidosPisoActualSubir:
;			move	R5, [CONTROLO]
;			and		R5, 070h				;bits de sensores de botoes exteriores para subir
;			shr		R5, 4
;			move	R4, R5
;			cmpl	R4
;			and		R5, R1
;			cnz		abre_porta
;			jmp		PedidoPisoActualRepouso
;		
;		PedidoPisoActualRepouso:
;			and		R0, R4
;			and		R0, R1	
;			cnz		abre_porta
;			cmp		R0, 0h				;compara R0 com 0, se for 0 é porque não teve pedidos no mesmo piso então passa para a frente
;			jnz		FimRotinaRepouso	;se não for 0 passa para o fim pois já teve pedidos no mesmo piso e já abriu a porta
;		
;		
		;fim teste
	
	PedidoPisoActualRepouso:
		and		R0, R1	
		cnz		abre_porta
		cmp		R0, 0h				;compara R0 com 0, se for 0 é porque não teve pedidos no mesmo piso então passa para a frente
		jnz		FimRotinaRepouso	;se não for 0 passa para o fim pois já teve pedidos no mesmo piso e já abriu a porta
		
	PedidoPisoInferiorRepouso:
		rorc	R1, 1
		jc		PedidoPisoSuperiorRepouso
		rorc	R2, 1
		jnc		PedidoPisoInferiorRepouso
		call	acciona_motor_descer
		jmp		FimRotinaRepouso
			
	PedidoPisoSuperiorRepouso:
		;move	R1, [0ffffh]
		;shr		R1, 12					;bits de sensores de piso actual - temos em R1 o piso actual
		cmp		R4, R3
		cn		acciona_motor_subir
		
	FimRotinaRepouso:
		ret	
	


;************************
;*	 Rotina Movimento	*
;************************
RotinaMovimento:
	move	R0, [0ffffh]
	and		R0, piso					;temos em R0 o piso actual
	jz		FimRotinaMovimento
	move	R0, [DIRECCAO]
	cmp		R0, descer
	jnz		PedidosSubir				;se a direcção for subir só vê se há pedidos para subir nesse piso
	
	
	PedidosDescer:
		move	R1, [CONTROLO]
		and		R1, 0fh
		move	R2, [CONTROLO]
		and		R2, 0f00h
		shr		R2, 8
		or		R1, R2						;temos em R1 todos os pedidos na direcção descer
		move	R0, [0ffffh]
		and		R0, piso					;temos em R0 o piso actual
		shr		R0, 12
		cmp		R1, 0h
		jz		UnicosPedidos
		and		R1, R0			
		cnz		para_motor
		move	R4, [DIRECCAO]
		cmp		R4, descer
		jz		FimRotinaMovimento
		
	PedidosSubir:
		move	R1, [CONTROLO]
		and		R1, 0f0h
		shr		R1, 4
		move	R2, [CONTROLO]
		and		R2, 0f00h
		shr		R2, 8
		or		R1, R2						;temos em R1 todos os pedidos na direcção subir
		move	R0, [0ffffh]
		and		R0, piso					;temos em R0 o piso actual
		shr		R0, 12
		cmp		R1, 0h
		jz		UnicosPedidos
		and		R1, R0						
		jz		FimRotinaMovimento
		call	para_motor
		jmp		FimRotinaMovimento
		
	UnicosPedidos:
		move	R1, [CONTROLO]
		and		R1, 0f0h
		shr		R1, 4
		move	R2, [CONTROLO]
		and		R2, 0f00h
		shr		R2, 8
		or		R1, R2				;R1 - Pedidos para subir e de dentro do elevador
		move	R2, [CONTROLO]
		and		R2, 0fh
		or		R1, R2				;R1 - Pedidos subir, dentro e descer
		move	R0, [0ffffh]
		and		R0, piso
		shr		R0, 12				;R0 - Piso actual
		and		R1, R0
		cnz		para_motor
		jmp		FimRotinaMovimento	
		
	FimRotinaMovimento:
		ret
	
	
;********************************
;*		Rotina Porta Aberta		*
;********************************
RotinaPortaAberta:
	move	R0, 1h
	cmp		[PORTA], R0 			;compara a variável PORTA com 1
	jnz		FimRotinaPortaAberta	;se for 1 passa para o fim da rotina porta aberta e retorna 
	move	R0, 0h
	move	[PORTA], R0				;coloca a variável PORTA a 0
	move	[0fffch], R0			;coloca o porto de controlo do rtclock a 0
	call	fecha_porta
	move	R0, repouso
	move	[ESTADO], R0
	
	FimRotinaPortaAberta:
		ret


;****************************
;*		ROTINA STOP			*
;****************************
RotinaStop:
	move	R0, [DIRECCAO]
	move	[OLDDIRECCAO], R0
	move	R0,	[ESTADO]
	move	[OLDESTADO], R0
	move	R0, 0011000000000000b
	and	    R0, [CONTROLO]
	move	[OLDMOTOR], R0
	move	R0, 1100111111111111b
	and		[CONTROLO], R0
	and		[0ffffh], R0
	move	R0, 1h
	move	[STOPVALUE], R0
		
	FuncaoStop:
		call	BotoesPiso
		call	EscreveControlo
		move	R0, [0ffffh]		
		and		R0, 0f00h				;em R0 temos os botões já carregados
		jz		FuncaoStop
		or		[CONTROLO], R0
		move	R0, [OLDDIRECCAO]
		move	[DIRECCAO], R0
		move	R0, [OLDESTADO]
		move	[ESTADO], R0
		move	R0, [OLDMOTOR]
		or		[CONTROLO], R0
		ret
	

	
	
;****************************************
;*		FUNÇÕES USADAS PELAS ROTINAS	*
;****************************************
abre_porta:
	call	apaga_luzes_piso
	call	EscreveControlo
	move	R5, 0000000000000001b
	or		[CONTROLO], R5
	move	R5, portaaberta				;passa para o estado porta aberta
	move	[ESTADO], R5
	move	R0, 3000d					;coloca o RTC a 3000
	move	[0fffch], R0
	move	R0, 0d
	move	[PORTA], R0					;coloca a variável PORTA a 0
	call	apaga_luzes_piso
	ret

fecha_porta:
	move	R5, 1111111111111110b		;fechaporta
	and		[CONTROLO], R5
	move	R5, repouso
	move	[ESTADO], R5
	ret

para_motor:
	move	R5, 1100111111111111b
	and		[CONTROLO], R5
	call	abre_porta
	ret
		
acciona_motor_subir:
	call	fecha_porta
	move	R5, motorsubir
	or		[CONTROLO], R5
	move	R5, movimento		;teste		
	move	[ESTADO], R5
	move	R5, subir
	move	[DIRECCAO], R5
	ret
		
acciona_motor_descer:
	call	fecha_porta
	move	R5, motordescer
	or		[CONTROLO], R5
	move	R5, movimento		;teste
	move	[ESTADO], R5
	move	R5, descer
	move	[DIRECCAO], R5
	ret
	
	
	
;********************************
;*		INDICADOR DE PISO		*
;********************************
actualiza_indicador_piso:	
	move	R0, [0ffffh]
	and		R0, piso
	move	R1, R0						;move para o registo R1 o que estava em R0 ou seja o piso em que está
	and		R1, piso0					;verifica se o elevador está no piso 0
	jnz		acende_indicador_piso0		;se não for 0 é pq o elevador está nesse piso chama a função de acender indicador desse piso
	move	R1, R0						;move para o registo R1 o que estava em R0 ou seja o piso em que está
	and		R1, piso1					;verifica se o elevador está no piso 1
	jnz		acende_indicador_piso1		;se não for 0 é pq o elevador está nesse piso chama a função de acender indicador desse piso
	move	R1, R0						;move para o registo R1 o que estava em R0 ou seja o piso em que está
	and		R1, piso2					;verifica se o elevador está no piso 2
	jnz		acende_indicador_piso2		;se não for 0 é pq o elevador está nesse piso chama a função de acender indicador desse piso
	move	R1, R0						;move para o registo R1 o que estava em R0 ou seja o piso em que está
	and		R1, piso3					;verifica se o elevador está no piso 3
	jnz		acende_indicador_piso3		;se não for 0 é pq o elevador está nesse piso chama a função de acender indicador desse piso
	jmp		fim_actualiza_indicador_piso
	
	acende_indicador_piso0:				;acende o indicador de piso 0 na cabine mantendo as acções a efectuar
		move	R5,	[CONTROLO]
		and		R5, 0011111111111111b
		move	[CONTROLO], R5
		jmp		fim_actualiza_indicador_piso
		
	acende_indicador_piso1:				;acende o indicador de piso 1 na cabine mantendo as acções a efectuar
		move	R5,	[CONTROLO]
		and		R5, 0011111111111111b
		or		R5, 0100000000000000b	
		move	[CONTROLO], R5
		jmp		fim_actualiza_indicador_piso
		
	acende_indicador_piso2:				;acende o indicador de piso 2 na cabine mantendo as acções a efectuar
		move	R5,	[CONTROLO]
		and		R5, 0011111111111111b
		or		R5, 1000000000000000b	
		move	[CONTROLO], R5
		jmp		fim_actualiza_indicador_piso
		
	acende_indicador_piso3:				;acende o indicador de piso 3 na cabine mantendo as acções a efectuar
		move	R5,	[CONTROLO]
		or		R5, 1100000000000000b
		move	[CONTROLO], R5
		jmp		fim_actualiza_indicador_piso
				
	fim_actualiza_indicador_piso:
		ret


;********************************
;*		APAGA LUZES DO PISO		*
;********************************
apaga_luzes_piso:
	move	R0, [0ffffh]
	and		R0, piso
	move	R1, R0					;temos em R0 e R1 o piso em que estamos
	and		R1, piso0
	jnz		apaga_luzes_piso0
	move	R1, R0
	and		R1, piso1
	jnz		apaga_luzes_piso1
	move	R1, R0
	and		R1, piso2
	jnz		apaga_luzes_piso2
	move	R1, R0
	and		R1, piso3
	jnz		apaga_luzes_piso3
	jmp		fim_apaga_luzes_piso
	
	apaga_luzes_piso0:				;apaga as luzes do piso 0
		move	R2, 1111111011101111b 
		and		[CONTROLO], R2
		jmp		fim_apaga_luzes_piso
		
	apaga_luzes_piso1:
		move	R3, [DIRECCAO]
		cmp		R3, descer
		jnz		apaga_luzes_piso1_subir
		
	
		
		apaga_luzes_piso1_descer:
			move	R1, [CONTROLO]
			and		R1, 0000000000001100b						;pedidos de descer excepto o do piso 1
			move	R2, [CONTROLO]
			and		R2, 0000111100000000b
			shr		R2, 8						;pedidos de dentro do elevador
			or		R1, R2						;temos em R1 todos os pedidos na direcção descer
			cmp		R1, 0h
			jz		unicas_luzes_piso1
			move	R0, [0ffffh]
			and		R0, piso					;temos em R0 o piso actual
			shr		R0, 12
			move	R2, 1111110111111101b 
			and		[CONTROLO], R2
			jmp		fim_apaga_luzes_piso
			
		apaga_luzes_piso1_subir:
			move	R1, [CONTROLO]
			and		R1, 0000000001010000b
			shr		R1, 4						;pedidos para subir excepto o do piso 1
			move	R2, [CONTROLO]
			and		R2, 0000111100000000b
			shr		R2, 8						;pedidos de dentro do elevador
			or		R1, R2						;temos em R1 todos os pedidos na direcção subir
			cmp		R1, 0h
			jz		unicas_luzes_piso1
			move	R0, [0ffffh]
			and		R0, piso					;temos em R0 o piso actual
			shr		R0, 12
			cmp		R1, 0h
			jz		unicas_luzes_piso2
			move	R2, 1111110111011111b 
			and		[CONTROLO], R2
			jmp		fim_apaga_luzes_piso
		
		unicas_luzes_piso1:
			move	R2, 1111110111011101b 
			and		[CONTROLO], R2
			jmp		fim_apaga_luzes_piso
		 
		 
	apaga_luzes_piso2:
		move	R3, [DIRECCAO]
		cmp		R3, descer
		jnz		apaga_luzes_piso2_subir
		
		
		apaga_luzes_piso2_descer:
			move	R1, [CONTROLO]
			and		R1, 0000000000001010b						;pedidos para descer excepto o do piso 2
			move	R2, [CONTROLO]
			and		R2, 0000111100000000b
			shr		R2, 8
			or		R1, R2						;temos em R1 todos os pedidos na direcção descer
			cmp		R1, 0h
			jz		unicas_luzes_piso2
			move	R0, [0ffffh]
			and		R0, piso					;temos em R0 o piso actual
			shr		R0, 12
			move	R2, 1111101111111011b 
			and		[CONTROLO], R2
			jmp		fim_apaga_luzes_piso
		
		apaga_luzes_piso2_subir:
			move	R1, [CONTROLO]
			and		R1, 0000000000110000b					;pedidos para subir excepto o do piso 2
			shr		R1, 4
			move	R2, [CONTROLO]
		and		R2, 0000111100000000b
			shr		R2, 8
			or		R1, R2						;temos em R1 todos os pedidos na direcção subirr
			cmp		R1, 0h
			jz		unicas_luzes_piso2
			move	R0, [0ffffh]
			and		R0, piso					;temos em R0 o piso actual
			shr		R0, 12
			cmp		R1, 0h
			jz		unicas_luzes_piso2
			move	R2, 1111101110111111b 
			and		[CONTROLO], R2
			jmp		fim_apaga_luzes_piso
			
		unicas_luzes_piso2:
			move	R2, 1111101110111011b 
			and		[CONTROLO], R2
			jmp		fim_apaga_luzes_piso
			
	apaga_luzes_piso3:
		move	R2, 1111011111110111b 
		and		[CONTROLO], R2
		jmp		fim_apaga_luzes_piso
	
	fim_apaga_luzes_piso:
		ret

;************************************	
;*		ROTINA ESCREVE CONTROLO		*
;************************************
EscreveControlo:
	move	R5, [CONTROLO]
	move	[0ffffh], R5
	ret
	
	
	
;****************
;*	 THE END	*	
;****************

ORIG	0fff0h
        jmp Inicio