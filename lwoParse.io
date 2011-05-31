/*
Copyright (c) 2011, Oscar Martinez
All rights reserved.
*/

SequenceCursor := Sequence clone do (
	cursor ::= 0
	isAtEnd ::= false

	with := method(aSeq,
		self clone appendSeq(aSeq)
	)

	read := method(n,
		isAtEnd ifFalse(
			r := exSlice(cursor, cursor + n) appendProto(SequenceCursor)
			advance(n)
			return r
		)
		nil
	)

	sliceFromCursor := method(
		exSlice(cursor, size) appendProto(SequenceCursor)
	)

	advance := method(n,
		isAtEnd ifFalse(
			cursor = cursor + n
			if(cursor >= size, isAtEnd = true)
		)
		self
	)
)

RawObjectSurf := Object clone do (
	COLR ::= nil
	DIFF ::= nil
	LUMI ::= nil
	SPEC ::= nil
	REFL ::= nil
	TRAN ::= nil
	TRNL ::= nil
	GLOS ::= nil
	SHRP ::= nil
	BUMP ::= nil
	SIDE ::= nil
	SMAN ::= nil
	RFOP ::= nil
	RIMG ::= nil
	RSAN ::= nil
	RBLR ::= nil
	RIND ::= nil
	TROP ::= nil
	TIMD ::= nil
	TBLR ::= nil
	CLRH ::= nil
	CLRF ::= nil
	ADTR ::= nil
	GLOW ::= nil
	LINE ::= nil
	ALPH ::= nil
	VCOL ::= nil
)

RawObjectBBox := Object clone do (
	min ::= nil
	max ::= nil
)

RawObjectLayer := Object clone do (
	number ::= -1
	flags ::= 0
	pivot ::= Vector clone
	name ::= ""
	parent ::= -1

	points ::= nil
	pols ::= nil
	surfs := Map clone
	uvMaps := Map clone
	uvMaps := Map clone with("vmap", Map clone, "vmad", Map clone)
	tags ::= nil
	surfTags ::= nil
	bbox ::= nil
)


RawObject := Object clone do (
	layers := List clone
	defaultLayer := RawObjectLayer clone

	lastLayer := method(
		if(layers isEmpty,
			return defaultLayer
		,
			return layers last
		)
	)
)


decodeVX := method(sc,
	n := 0
	if(sc at(0) == 255, //0xFFxxxxxx
		n = sc unpack(sc cursor, "*I") first & 0x00FFFFFF ; sc advance(4)
	,
		n = sc unpack(sc cursor, "*H") first ; sc advance(2)
	)
	n
)


decodeS0 := method(sc,
	s := sc unpack(sc cursor, "s") first
	sc advance(s size + 1)
	if( (s size + 1) isOdd, sc advance(1) )
	s
)

Chunk := Object clone do (
	length ::= 0
	read := method(seq, root,
		setLength(seq unpack(seq cursor, "*I") first) ; seq advance(4)
		if(length isOdd, setLength(length + 1))
		writeln("[" .. self type .. "] " .. length)
		?decode(seq read(self length), root)
		self
	)
)

SubChunk := Object clone do (
	length ::= 0
	read := method(seq, root,
		setLength(seq unpack(seq cursor, "*H") first) ; seq advance(2)
		if(length isOdd, setLength(length + 1))
		writeln("[" .. self type .. "] " .. length)
		?decode(seq read(self length), root)
		self
	)

	decode := method(sc, root,
		self type println
		self
	)
)



/*
TAGS
LAYR
PNTS
BBOX
VMAP
POLS
PTAG
VMAD
CLIP
SURF
*/

TAGS := Chunk clone do (
	decode := method(sc, root,
		tags := List clone

		while(sc isAtEnd not,
			s := decodeS0(sc)
			tags append(s)
			s println
		)

		root lastLayer setTags(tags)
		self
	)
)

LAYR := Chunk clone do (
	// LAYR { number[U2], flags[U2], pivot[VEC12], name[S0], parent[U2] ? }
	decode := method(sc, root,
		//l := sc unpack("*" .. U2 .. U2 .. VEC12 .. S0 .. U2)
		l := sc unpack(sc cursor, "*HHfffss")

		layer := RawObjectLayer clone

		layer setNumber(l at(0))
		layer setFlags(l at(1))
		layer setPivot(Vector clone setX(l at(2)) setY(l at(3)) setZ(l at(4)))
		layer setName(l at(5))
		if(l size > 6, layer setParent(l at(0)) )

		l println

		root layers append(layer)
		self
	)
)

PNTS := Chunk clone do (
	decode := method(sc, root,
		points := List clone

		while(sc isAtEnd not,
			points append(sc unpack(sc cursor, "*fff"))
			sc advance(12)
		)

		root lastLayer setPoints(points)
		self
	)
)

BBOX := Chunk clone do (
	decode := method(sc, root,
		min := Vector clone
		max := Vector clone

		l := sc read(12) unpack("*fff")
		min setX(l at(0)) setY(l at(1)) setZ(l at(2))
		l = sc read(12) unpack("*fff")
		max setX(l at(0)) setY(l at(1)) setZ(l at(2))

		min println
		max println

		bbox := RawObjectBBox clone
		bbox setMin(min)
		bbox setMax(max)

		root lastLayer setBbox(bbox)
		self
	)
)

VMAP := Chunk clone do (
	// VMAP { type[ID4], dimension[U2], name[S0], ( vert[VX], value[F4] # dimension )* }

	TXUV := method(sc, root,
		m := Map clone
		while(sc isAtEnd not,
			m atPut(decodeVX(sc) asString, sc unpack(sc cursor, "*ff"))
			sc advance(8)
		)
		m
	)

	/*PICK
	WGHT
	MNVW
	TXUV
	RGB, RGBA
	MORF
	SPOT*/

	decode := method(sc, root,
		t := self getSlot(sc read(4))
		d := sc read(2) unpack("*H") first
		n := decodeS0(sc) ; n println

		root lastLayer uvMaps at("vmap") atPut(n, t(sc, root))
		self
	)
)

POLS := Chunk clone do (
	// POLS { type[ID4], ( numvert+flags[U2], vert[VX] # numvert )* }

	FACE := method(sc, root,
		pols := List clone
		verts := nil
		nv := nil
		while(sc isAtEnd not,
			verts = List clone
			nv = sc unpack(sc cursor, "*H") first & 0x3FF ; sc advance(2)
			nv repeat(
				verts append(decodeVX(sc))
			)
			pols append(verts)
		)

		pols
	)

	/*CURV
	PTCH
	MBAL
	BONE*/


	decode := method(sc, root,
		t := self getSlot(sc read(4))

		root lastLayer setPols(t(sc, root))
		self
	)
)

PTAG := Chunk clone do (
	//PTAG { type[ID4], ( poly[VX], tag[U2] )* }

	SURF := method(sc, root,
		st := List clone setSize(root lastLayer pols size)
		while(sc isAtEnd not,
			st atPut(decodeVX(sc), sc unpack(sc cursor, "*H") first)
			sc advance(2)
		)
		st
	)

	/*PART
	SMGP*/

	decode := method(sc, root,
		t := self getSlot(sc read(4))
		root lastLayer setSurfTags(t(sc, root))
		self
	)
)

VMAD := Chunk clone do (
	//VMAD { type[ID4], dimension[U2], name[S0], ( vert[VX], poly[VX], value[F4] # dimension )* }

	TXUV := method(sc, root,
		vert := nil
		poly := nil
		uv := nil
		m := Map clone
		while(sc isAtEnd not,
			vert = decodeVX(sc) asString
			poly = decodeVX(sc) asString
			uv = sc unpack(sc cursor, "*ff")

			//Map clone???
			m atPut(poly, Map clone atPut(vert, uv))
			sc advance(8)
		)
		m
	)

	/*
	WGHT
	RGB
	RGBA
	*/

	decode := method(sc, root,
		t := self getSlot(sc read(4))
		d := sc read(2) unpack("H") first
		n := decodeS0(sc) ; n println

		root lastLayer uvMaps at("vmad") atPut(n, t(sc, root))
		self
	)
)

CLIP := Chunk clone do (
	//CLIP { index[U4], attributes[SUB-CHUNK] * }

	STIL := SubChunk clone do (
		//STIL { name[FNAM0] }
		/*filename ::= nil
		decode := method(sc, root,
			setFilename(decodeS0(sc))
			filename println
			self
		)*/
	)

	XREF := SubChunk clone do (
		//XREF { index[U4], string[S0] }
	)

	TIME := SubChunk clone do (
		//TIME { start-time[FP4], duration[FP4], frame-rate[FP4] }
	)

	CLRS := SubChunk clone do (
		//CLRS { flags[U2], colorspace[U2], filename[FNAM0] }
	)

	CLRA := SubChunk clone do (
		//CLRA { flags[U2], colorspace[U2], filename[FNAM0] }
	)

	FILT := SubChunk clone do (
		//FILT { flags[U2] }
	)

	DITH := SubChunk clone do (
		//DITH { flags[U2] }
	)

	CONT := SubChunk clone do (
		//CONT { contrast-delta[FP4], envelope[VX] }
	)

	BRIT := SubChunk clone do (
		//BRIT { brightness-delta[FP4], envelope[VX] }
	)

	SATR := SubChunk clone do (
		//SATR { saturation-delta[FP4], envelope[VX] }
	)

	HUE := SubChunk clone do (
		//HUE { hue-rotation[FP4], envelope[VX] }
	)

	GAMM := SubChunk clone do (
		//GAMM { gamma[F4], envelope[VX] }
	)

	NEGA := SubChunk clone do (
		//NEGA { enable[U2] }
	)

	IFLT := SubChunk clone do (
		//IFLT { server-name[S0], flags[U2], data[...] }
	)

	PFLT := SubChunk clone do (
		//PFLT { server-name[S0], flags[U2], data[...] }
	)


	decode := method(sc, root,
		//setIndex(sc read(4) unpack("*I") first asString)
		//subcnk := sc read(4)
		//subcnk println
		//clips atPut(index, self getSlot(subcnk) clone read(sc))

		self
	)
)


FLAG := Chunk clone do (
	decode := method(sc, root,
		sc read(4) unpack("*I") first println
		self
	)
)

SURF := Chunk clone do (
	//SURF { name[S0], source[S0], attributes[SUB-CHUNK] * }

	COLR := SubChunk clone do (
		//COLR { base-color[COL12], envelope[VX] }
		decode := method(sc, root, sc unpack("*fff"))
	)
	DIFF := SubChunk clone do (
		//DIFF { intensity[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f"))
	)
	LUMI := SubChunk clone do (
		//LUMI { intensity[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f"))
	)
	SPEC := SubChunk clone do (
		//SPEC { intensity[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f"))
	)
	REFL := SubChunk clone do (
		//REFL { intensity[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f"))
	)
	TRAN := SubChunk clone do (
		//TRAN { intensity[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f"))
	)
	TRNL := SubChunk clone do (
		//TRNL { intensity[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f"))
	)
	GLOS := SubChunk clone do (
		//GLOS { glossiness[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f"))
	)
	SHRP := SubChunk clone do (
		//SHRP { sharpness[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f"))
	)
	BUMP := SubChunk clone do (
		//BUMP { strength[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f"))
	)
	SIDE := SubChunk clone do (
		//SIDE { sidedness[U2] }
		decode := method(sc, root, sc unpack("*f"))
	)
	SMAN := SubChunk clone do (
		//SMAN { max-smoothing-angle[ANG4] }
		decode := method(sc, root, sc unpack("*f"))
	)
	RFOP := SubChunk clone do (
		//RFOP { reflection-options[U2] }
		decode := method(sc, root, sc unpack("*f"))
	)
	RIMG := SubChunk clone do (
		//RIMG { image[VX] }
		decode := method(sc, root, sc unpack("*f"))
	)
	RSAN := SubChunk clone do (
		//RSAN { seam-angle[ANG4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f"))
	)
	RBLR := SubChunk clone do (
		//RBLR { blur-percentage[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f"))
	)
	RIND := SubChunk clone do (
		//RIND { refractive-index[F4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f"))
	)
	TROP := SubChunk clone do (
		//TROP { transparency-options[U2] }
		decode := method(sc, root, sc unpack("*H"))
	)
	TIMD := SubChunk clone do (
		//TIMG { image[VX] }
		decode := method(sc, root, decodeVX(sc))
	)
	TBLR := SubChunk clone do (
		//TBLR { blur-percentage[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f"))
	)
	CLRH := SubChunk clone do (
		//CLRH { color-highlights[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f"))
	)
	CLRF := SubChunk clone do (
		//CLRF { color-filter[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f"))
	)
	ADTR := SubChunk clone do (
		//ADTR { additive[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f"))
	)
	GLOW := SubChunk clone do (
		//GLOW { type[U2], intensity[F4], intensity-envelope[VX], size[F4], size-envelope[VX] }
	)
	LINE := SubChunk clone do (
		//LINE { flags[U2], ( size[F4], size-envelope[VX], ( color[COL12], color-envelope[VX] )? )? }
	)
	ALPH := SubChunk clone do (
		//ALPH { mode[U2], value[FP4] }
	)
	VCOL := SubChunk clone do (
		//VCOL { intensity[FP4], envelope[VX], vmap-type[ID4], name[S0] }
	)

	//BLOK := SubChunk clone do (
	//)

	decode := method(sc, root,
		n := decodeS0(sc) ; n println
		s := decodeS0(sc) ; s println
		surf := RawObjectSurf clone
		t := nil
		while(sc isAtEnd not,
			subcnk := sc read(4)
			t = self getSlot(subcnk)
			t isNil ifFalse(surf updateSlot(subcnk, t read(sc, root)))
		)

		root lastLayer surfs atPut(n, surf)

		self
	)
)

FORM := Chunk clone do (
	LWO2 := Object clone do (
		TAGS := Lobby getSlot("TAGS")
		LAYR := Lobby getSlot("LAYR")
		PNTS := Lobby getSlot("PNTS")
		BBOX := Lobby getSlot("BBOX")
		VMAP := Lobby getSlot("VMAP")
		POLS := Lobby getSlot("POLS")
		PTAG := Lobby getSlot("PTAG")
		VMAD := Lobby getSlot("VMAD")
		CLIP := Lobby getSlot("CLIP")
		SURF := Lobby getSlot("SURF")
	)

	decode := method(sc, root,
		lwo := self getSlot(sc read(4))
		lwo type println

		while(sc isAtEnd not,
			chunkid := sc read(4)
			chunk := lwo getSlot(chunkid)
			chunk read(sc, root)
		)
	)
)



LwoObject := Object clone do (

	read := method(fileName,
		f := File with(fileName) openForReading

		sc := SequenceCursor with(f contents)

		Collector setAllocsPerSweep(f size / 4)

		f close

		obj := RawObject clone

		form := Lobby getSlot(sc read(4))
		sc size println
		form read(sc, obj)
		sc size println

		obj layers println

		obj
	)
)


//fileName := "sponza.lwo"

obj := LwoObject read(System args at(1))

obj layers foreach(l,
	l println
)
