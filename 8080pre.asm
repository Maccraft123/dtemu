; z80dasm 1.1.6
; command line: z80dasm -a ./Pobrane/8080PRE.COM

	org	00100h

	ld a,001h		;0100
	cp 002h		;0102
	jp z,00000h		;0104
	cp 001h		;0107
	jp nz,00000h		;0109
	jp 00111h		;010c
	halt			;010f
	rst 38h			;0110
	call 00117h		;0111
	jp 00000h		;0114
	pop hl			;0117
	ld a,h			;0118
	cp 001h		;0119
	jp z,00121h		;011b
	jp 00000h		;011e
	ld a,l			;0121
	cp 014h		;0122
	jp z,0012ah		;0124
	jp 00000h		;0127
	ld sp,00399h		;012a
	pop af			;012d
	pop bc			;012e
	pop de			;012f
	pop hl			;0130
	ld sp,003a9h		;0131
	push hl			;0134
	push de			;0135
	push bc			;0136
	push af			;0137
	ld a,(003a1h)		;0138
	cp 002h		;013b
	jp nz,00000h		;013d
	ld a,(003a2h)		;0140
	cp 004h		;0143
	jp nz,00000h		;0145
	ld a,(003a3h)		;0148
	cp 006h		;014b
	jp nz,00000h		;014d
	ld a,(003a4h)		;0150
	cp 008h		;0153
	jp nz,00000h		;0155
	ld a,(003a5h)		;0158
	cp 00ah		;015b
	jp nz,00000h		;015d
	ld a,(003a6h)		;0160
	cp 00ch		;0163
	jp nz,00000h		;0165
	ld a,(003a7h)		;0168
	cp 00eh		;016b
	jp nz,00000h		;016d
	ld a,(003a8h)		;0170
	cp 010h		;0173
	jp nz,00000h		;0175
	ld hl,003a9h		;0178
	ld a,(hl)			;017b
	cp 0a5h		;017c
	jp nz,00000h		;017e
	ld hl,003aah		;0181
	ld a,(hl)			;0184
	cp 03ch		;0185
	jp nz,00000h		;0187
	ld sp,00500h		;018a
	ld hl,00195h		;018d
	push hl			;0190
	ret			;0191
	jp 00000h		;0192
	ld a,0ffh		;0195
	and 00fh		;0197
	cp 00fh		;0199
	jp nz,00000h		;019b
	ld a,05ah		;019e
	and 00fh		;01a0
	cp 00ah		;01a2
	jp nz,00000h		;01a4
	rrca			;01a7
	cp 005h		;01a8
	jp nz,00000h		;01aa
	rrca			;01ad
	cp 082h		;01ae
	jp nz,00000h		;01b0
	rrca			;01b3
	cp 041h		;01b4
	jp nz,00000h		;01b6
	rrca			;01b9
	cp 0a0h		;01ba
	jp nz,00000h		;01bc
	ld hl,01234h		;01bf
	push hl			;01c2
	pop bc			;01c3
	ld a,b			;01c4
	cp 012h		;01c5
	jp nz,00000h		;01c7
	ld a,c			;01ca
	cp 034h		;01cb
	jp nz,00000h		;01cd
	ld hl,00001h		;01d0
	push hl			;01d3
	pop af			;01d4
	call c,001dbh		;01d5
	jp 00352h		;01d8
	pop hl			;01db
	ld hl,000d6h		;01dc
	push hl			;01df
	pop af			;01e0
	call nc,001e7h		;01e1
	jp 00352h		;01e4
	pop hl			;01e7
	ld hl,001f5h		;01e8
	push hl			;01eb
	ld hl,00001h		;01ec
	push hl			;01ef
	pop af			;01f0
	ret c			;01f1
	call 00352h		;01f2
	ld hl,00202h		;01f5
	push hl			;01f8
	ld hl,000d6h		;01f9
	push hl			;01fc
	pop af			;01fd
	ret nc			;01fe
	call 00352h		;01ff
	ld hl,00001h		;0202
	push hl			;0205
	pop af			;0206
	jp c,0020dh		;0207
	call 00352h		;020a
	ld hl,000d6h		;020d
	push hl			;0210
	pop af			;0211
	jp nc,00218h		;0212
	call 00352h		;0215
	ld hl,00004h		;0218
	push hl			;021b
	pop af			;021c
	call pe,00223h		;021d
	jp 00352h		;0220
	pop hl			;0223
	ld hl,000d3h		;0224
	push hl			;0227
	pop af			;0228
	call po,0022fh		;0229
	jp 00352h		;022c
	pop hl			;022f
	ld hl,0023dh		;0230
	push hl			;0233
	ld hl,00004h		;0234
	push hl			;0237
	pop af			;0238
	ret pe			;0239
	call 00352h		;023a
	ld hl,0024ah		;023d
	push hl			;0240
	ld hl,000d3h		;0241
	push hl			;0244
	pop af			;0245
	ret po			;0246
	call 00352h		;0247
	ld hl,00004h		;024a
	push hl			;024d
	pop af			;024e
	jp pe,00255h		;024f
	call 00352h		;0252
	ld hl,000d3h		;0255
	push hl			;0258
	pop af			;0259
	jp po,00260h		;025a
	call 00352h		;025d
	ld hl,00040h		;0260
	push hl			;0263
	pop af			;0264
	call z,0026bh		;0265
	jp 00352h		;0268
	pop hl			;026b
	ld hl,00097h		;026c
	push hl			;026f
	pop af			;0270
	call nz,00277h		;0271
	jp 00352h		;0274
	pop hl			;0277
	ld hl,00285h		;0278
	push hl			;027b
	ld hl,00040h		;027c
	push hl			;027f
	pop af			;0280
	ret z			;0281
	call 00352h		;0282
	ld hl,00292h		;0285
	push hl			;0288
	ld hl,00097h		;0289
	push hl			;028c
	pop af			;028d
	ret nz			;028e
	call 00352h		;028f
	ld hl,00040h		;0292
	push hl			;0295
	pop af			;0296
	jp z,0029dh		;0297
	call 00352h		;029a
	ld hl,00097h		;029d
	push hl			;02a0
	pop af			;02a1
	jp nz,002a8h		;02a2
	call 00352h		;02a5
	ld hl,00080h		;02a8
	push hl			;02ab
	pop af			;02ac
	call m,002b3h		;02ad
	jp 00352h		;02b0
	pop hl			;02b3
	ld hl,00057h		;02b4
	push hl			;02b7
	pop af			;02b8
	call p,002bfh		;02b9
	jp 00352h		;02bc
	pop hl			;02bf
	ld hl,002cdh		;02c0
	push hl			;02c3
	ld hl,00080h		;02c4
	push hl			;02c7
	pop af			;02c8
	ret m			;02c9
	call 00352h		;02ca
	ld hl,002dah		;02cd
	push hl			;02d0
	ld hl,00057h		;02d1
	push hl			;02d4
	pop af			;02d5
	ret p			;02d6
	call 00352h		;02d7
	ld hl,00080h		;02da
	push hl			;02dd
	pop af			;02de
	jp m,002e5h		;02df
	call 00352h		;02e2
	ld hl,00057h		;02e5
	push hl			;02e8
	pop af			;02e9
	jp p,002f0h		;02ea
	call 00352h		;02ed
	ld hl,002f7h		;02f0
	jp (hl)			;02f3
	call 00352h		;02f4
	ld a,0a5h		;02f7
	ld b,004h		;02f9
	rrca			;02fb
	dec b			;02fc
	jp nz,002fbh		;02fd
	cp 05ah		;0300
	call nz,00352h		;0302
	ld b,010h		;0305
	inc a			;0307
	dec b			;0308
	jp nz,00307h		;0309
	cp 06ah		;030c
	call nz,00352h		;030e
	ld b,000h		;0311
	ld hl,00000h		;0313
	inc hl			;0316
	dec b			;0317
	jp nz,00316h		;0318
	ld a,h			;031b
	cp 001h		;031c
	call nz,00352h		;031e
	ld a,l			;0321
	cp 000h		;0322
	call nz,00352h		;0324
	ld de,00332h		;0327
	ld c,009h		;032a
	call 00005h		;032c
	jp 00000h		;032f
	jr c,$+50		;0332
	jr c,$+50		;0334
	jr nz,$+82		;0336
	ld (hl),d			;0338
	ld h,l			;0339
	ld l,h			;033a
	ld l,c			;033b
	ld l,l			;033c
	ld l,c			;033d
	ld l,(hl)			;033e
	ld h,c			;033f
	ld (hl),d			;0340
	ld a,c			;0341
	jr nz,$+118		;0342
	ld h,l			;0344
	ld (hl),e			;0345
	ld (hl),h			;0346
	ld (hl),e			;0347
	jr nz,$+101		;0348
	ld l,a			;034a
	ld l,l			;034b
	ld (hl),b			;034c
	ld l,h			;034d
	ld h,l			;034e
	ld (hl),h			;034f
	ld h,l			;0350
	inc h			;0351
	pop bc			;0352
	ld h,004h		;0353
	ld a,b			;0355
	rrca			;0356
	rrca			;0357
	rrca			;0358
	rrca			;0359
	and 00fh		;035a
	ld l,a			;035c
	ld a,(hl)			;035d
	call 0038ah		;035e
	ld a,b			;0361
	and 00fh		;0362
	ld l,a			;0364
	ld a,(hl)			;0365
	call 0038ah		;0366
	ld a,c			;0369
	rrca			;036a
	rrca			;036b
	rrca			;036c
	rrca			;036d
	and 00fh		;036e
	ld l,a			;0370
	ld a,(hl)			;0371
	call 0038ah		;0372
	ld a,c			;0375
	and 00fh		;0376
	ld l,a			;0378
	ld a,(hl)			;0379
	call 0038ah		;037a
	ld a,00dh		;037d
	call 0038ah		;037f
	ld a,00ah		;0382
	call 0038ah		;0384
	jp 00000h		;0387
	push af			;038a
	push bc			;038b
	push de			;038c
	push hl			;038d
	ld c,002h		;038e
	ld e,a			;0390
	call 00005h		;0391
	pop hl			;0394
	pop de			;0395
	pop bc			;0396
	pop af			;0397
	ret			;0398
	ld (bc),a			;0399
	inc b			;039a
	ld b,008h		;039b
	ld a,(bc)			;039d
	inc c			;039e
	ld c,010h		;039f
	nop			;03a1
	nop			;03a2
	nop			;03a3
	nop			;03a4
	nop			;03a5
	nop			;03a6
	nop			;03a7
	nop			;03a8
	and l			;03a9
	inc a			;03aa
	nop			;03ab
	nop			;03ac
	nop			;03ad
	nop			;03ae
	nop			;03af
	nop			;03b0
	nop			;03b1
	nop			;03b2
	nop			;03b3
	nop			;03b4
	nop			;03b5
	nop			;03b6
	nop			;03b7
	nop			;03b8
	nop			;03b9
	nop			;03ba
	nop			;03bb
	nop			;03bc
	nop			;03bd
	nop			;03be
	nop			;03bf
	nop			;03c0
	nop			;03c1
	nop			;03c2
	nop			;03c3
	nop			;03c4
	nop			;03c5
	nop			;03c6
	nop			;03c7
	nop			;03c8
	nop			;03c9
	nop			;03ca
	nop			;03cb
	nop			;03cc
	nop			;03cd
	nop			;03ce
	nop			;03cf
	nop			;03d0
	nop			;03d1
	nop			;03d2
	nop			;03d3
	nop			;03d4
	nop			;03d5
	nop			;03d6
	nop			;03d7
	nop			;03d8
	nop			;03d9
	nop			;03da
	nop			;03db
	nop			;03dc
	nop			;03dd
	nop			;03de
	nop			;03df
	nop			;03e0
	nop			;03e1
	nop			;03e2
	nop			;03e3
	nop			;03e4
	nop			;03e5
	nop			;03e6
	nop			;03e7
	nop			;03e8
	nop			;03e9
	nop			;03ea
	nop			;03eb
	nop			;03ec
	nop			;03ed
	nop			;03ee
	nop			;03ef
	nop			;03f0
	nop			;03f1
	nop			;03f2
	nop			;03f3
	nop			;03f4
	nop			;03f5
	nop			;03f6
	nop			;03f7
	nop			;03f8
	nop			;03f9
	nop			;03fa
	nop			;03fb
	nop			;03fc
	nop			;03fd
	nop			;03fe
	nop			;03ff
	jr nc,$+51		;0400
	ld (03433h),a		;0402
	dec (hl)			;0405
	ld (hl),037h		;0406
	jr c,$+59		;0408
	ld h,c			;040a
	ld h,d			;040b
	ld h,e			;040c
	ld h,h			;040d
	ld h,l			;040e
	ld h,(hl)			;040f
	nop			;0410
	nop			;0411
	nop			;0412
	nop			;0413
	nop			;0414
	nop			;0415
	nop			;0416
	nop			;0417
	nop			;0418
	nop			;0419
	nop			;041a
	nop			;041b
	nop			;041c
	nop			;041d
	nop			;041e
	nop			;041f
	nop			;0420
	nop			;0421
	nop			;0422
	nop			;0423
	nop			;0424
	nop			;0425
	nop			;0426
	nop			;0427
	nop			;0428
	nop			;0429
	nop			;042a
	nop			;042b
	nop			;042c
	nop			;042d
	nop			;042e
	nop			;042f
	nop			;0430
	nop			;0431
	nop			;0432
	nop			;0433
	nop			;0434
	nop			;0435
	nop			;0436
	nop			;0437
	nop			;0438
	nop			;0439
	nop			;043a
	nop			;043b
	nop			;043c
	nop			;043d
	nop			;043e
	nop			;043f
	nop			;0440
	nop			;0441
	nop			;0442
	nop			;0443
	nop			;0444
	nop			;0445
	nop			;0446
	nop			;0447
	nop			;0448
	nop			;0449
	nop			;044a
	nop			;044b
	nop			;044c
	nop			;044d
	nop			;044e
	nop			;044f
	nop			;0450
	nop			;0451
	nop			;0452
	nop			;0453
	nop			;0454
	nop			;0455
	nop			;0456
	nop			;0457
	nop			;0458
	nop			;0459
	nop			;045a
	nop			;045b
	nop			;045c
	nop			;045d
	nop			;045e
	nop			;045f
	nop			;0460
	nop			;0461
	nop			;0462
	nop			;0463
	nop			;0464
	nop			;0465
	nop			;0466
	nop			;0467
	nop			;0468
	nop			;0469
	nop			;046a
	nop			;046b
	nop			;046c
	nop			;046d
	nop			;046e
	nop			;046f
	nop			;0470
	nop			;0471
	nop			;0472
	nop			;0473
	nop			;0474
	nop			;0475
	nop			;0476
	nop			;0477
	nop			;0478
	nop			;0479
	nop			;047a
	nop			;047b
	nop			;047c
	nop			;047d
	nop			;047e
	nop			;047f
	nop			;0480
	nop			;0481
	nop			;0482
	nop			;0483
	nop			;0484
	nop			;0485
	nop			;0486
	nop			;0487
	nop			;0488
	nop			;0489
	nop			;048a
	nop			;048b
	nop			;048c
	nop			;048d
	nop			;048e
	nop			;048f
	nop			;0490
	nop			;0491
	nop			;0492
	nop			;0493
	nop			;0494
	nop			;0495
	nop			;0496
	nop			;0497
	nop			;0498
	nop			;0499
	nop			;049a
	nop			;049b
	nop			;049c
	nop			;049d
	nop			;049e
	nop			;049f
	nop			;04a0
	nop			;04a1
	nop			;04a2
	nop			;04a3
	nop			;04a4
	nop			;04a5
	nop			;04a6
	nop			;04a7
	nop			;04a8
	nop			;04a9
	nop			;04aa
	nop			;04ab
	nop			;04ac
	nop			;04ad
	nop			;04ae
	nop			;04af
	nop			;04b0
	nop			;04b1
	nop			;04b2
	nop			;04b3
	nop			;04b4
	nop			;04b5
	nop			;04b6
	nop			;04b7
	nop			;04b8
	nop			;04b9
	nop			;04ba
	nop			;04bb
	nop			;04bc
	nop			;04bd
	nop			;04be
	nop			;04bf
	nop			;04c0
	nop			;04c1
	nop			;04c2
	nop			;04c3
	nop			;04c4
	nop			;04c5
	nop			;04c6
	nop			;04c7
	nop			;04c8
	nop			;04c9
	nop			;04ca
	nop			;04cb
	nop			;04cc
	nop			;04cd
	nop			;04ce
	nop			;04cf
	nop			;04d0
	nop			;04d1
	nop			;04d2
	nop			;04d3
	nop			;04d4
	nop			;04d5
	nop			;04d6
	nop			;04d7
	nop			;04d8
	nop			;04d9
	nop			;04da
	nop			;04db
	nop			;04dc
	nop			;04dd
	nop			;04de
	nop			;04df
	nop			;04e0
	nop			;04e1
	nop			;04e2
	nop			;04e3
	nop			;04e4
	nop			;04e5
	nop			;04e6
	nop			;04e7
	nop			;04e8
	nop			;04e9
	nop			;04ea
	nop			;04eb
	nop			;04ec
	nop			;04ed
	nop			;04ee
	nop			;04ef
	nop			;04f0
	nop			;04f1
	nop			;04f2
	nop			;04f3
	nop			;04f4
	nop			;04f5
	nop			;04f6
	nop			;04f7
	nop			;04f8
	nop			;04f9
	nop			;04fa
	nop			;04fb
	nop			;04fc
	nop			;04fd
	nop			;04fe
	nop			;04ff
