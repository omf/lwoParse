/*
Copyright (c) 2011, Oscar Martinez
All rights reserved.
*/

LinkList := List clone do (

	_append := getSlot("append")
	_at := getSlot("at")
	_remove := getSlot("remove")
	_reverse := getSlot("reverse")

	append := method(o,
		l := self last
		f := self first
		if(size > 0,
			o prev := l
			o next := f
			f prev = o
			l next = o
		,
			o prev := o
			o next := o
		)
		_append(o)
	)


	at := method(i,
		if(i < 0, i = size + i)
		if(i >= size, i = i % size)
		_at(i)
	)


	remove := method(o,
		o prev next = o next
		o next prev = o prev
		_remove(o)
	)

	reverse := method(
		l := self _reverse map(v, t := v prev ; v prev = v next ; v next = t ; v)
		l setProto(LinkList)
		l
	)
)

Vector asString := method(
	x .. " " .. y .. " " z
)
Vector asSimpleString := asString

Sequence u := method(at(Lobby u))
Sequence v := method(at(Lobby v))

Sequence perpDotProduct := method(aVec,
	//(self x * aVec y) - (self y * aVec x)
	(self u * aVec v) - (self v * aVec u)
)

Sequence crossProduct := method(aVec,
	v := Vector clone
	v setX(y * aVec z - z * aVec y)
	v setY(z * aVec x - x * aVec z)
	v setZ(x * aVec y - y * aVec x)
	v
)

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

	/*_unpack := getSlot("unpack")
	unpack := method(
		call delegateToMehod(self, "_upack")
	)*/
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
	BLOK ::= nil
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
	uvMaps := Map clone with("vmap", Map clone, "vmad", Map clone)
	surfTags ::= nil
	clips := Map clone
	bbox ::= nil
)


RawObject := Object clone do (
	tags ::= nil
	layers := List clone
	defaultLayer := RawObjectLayer clone

	lastLayer := method(
		if(layers isEmpty,
			return defaultLayer
		,
			return layers last
		)
	)

	getLayers := method(if(layers isEmpty return list(defaultLayer), return layers))
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
	)

	decode := method(sc, root, nil)
)

SubChunk := Object clone do (
	length ::= 0
	read := method(seq, root,
		setLength(seq unpack(seq cursor, "*H") first) ; seq advance(2)
		if(length isOdd, setLength(length + 1))
		writeln("(" .. self type .. ") " .. length)
		?decode(seq read(self length), root)
	)

	decode := method(sc, root, nil)
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

		root setTags(tags)
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
		layer setPivot(Vector clone setItemType("float32") setX(l at(2)) setY(l at(3)) setZ(l at(4)))
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
		layer := root lastLayer
		pivot := layer pivot
		while(sc isAtEnd not,
			p := sc unpack(sc cursor, "*fff")
			p = Vector clone setItemType("float32") setX(p at(0) - pivot x) setY(p at(1) - pivot y) setZ(p at(2) - pivot z)
			points append(p)
			sc advance(12)
		)

		layer setPoints(points)
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

	testOutside := method(p, pnts, reflx,
		reflx isEmpty ifTrue(return true)
		// P-A = b(B-A) + c(C-A)
		// R=P-A, S=B-A, T=C-A
		// R = bS + cT
		A := pnts at(p prev)
		S := pnts at(p) - A
		T := pnts at(p next) - A
		div := 1 / ((S u * T v) - (S v * T u))		//(SxTy - SyTx)

		reflx foreach(P,
			R := pnts at(P) - A
			b := ((R u * T v) - (R v * T u)) * div	//(RxTy - RyTx) / (SxTy - SyTx)
			c := ((S u * R v) - (S v * R u)) * div	//(SxRy - SyRx) / (SxTy - SyTx)
			(b <= 0 or c <= 0 or (b + c) > 1) ifFalse(return false)	//true == P is out
		)
		true
	)

	testReflex := method(p, pnts,
		A := pnts at(p prev)
		B := pnts at(p)
		C := pnts at(p next)
		(A - B) perpDotProduct(C - B) < 0
	)

	clockTest := method(verts, pnts,
		area := 0
		verts foreach(p,
			area = area + pnts at(p next) perpDotProduct(pnts at(p))
		)
		area //clockwise: <0
	)

	findDominantAxis := method(verts, pnts,
		v := verts first
		N := pnts at(v next) - pnts at(v)
		P := pnts at(v prev) - pnts at(v)
		C := N crossProduct(P) abs
		Lobby u := 1
		Lobby v := 2
		if(C y > C x,
			Lobby u := 0
			Lobby v := 2
		)
		if(C z > C y,
			Lobby u := 0
			Lobby v := 1
		)
	)

	triangulate := method(root, verts, pols,
		pnts := root lastLayer points
		reflx := List clone
		convex := List clone
		ears := List clone

		findDominantAxis(verts, pnts)
		
		(clockTest(verts, pnts) < 0) ifTrue(verts = verts reverse)
			
		verts foreach(p,
			if( testReflex(p, pnts), reflx append(p), convex append(p) )
		)

		convex foreach(p,
			testOutside(p, pnts, reflx) ifTrue(ears append(p))
		)

		if(reflx isEmpty,
			//convex polygon
			pivot := verts removeFirst
			n := verts removeFirst
			verts size repeat (
				tri := List clone
				tri append(pivot)
				tri append(n)
				n = verts removeFirst
				tri append(n)
				pols append(tri)
			)
		,
			while(verts size > 3,
				v := ears removeFirst
				a := v prev
				b := v next
				pols append(List with(a, v, b))
				verts remove(v)

				if(ears contains(a),
					testOutside(a, pnts, reflx) ifFalse(ears remove(a))
				,
					testReflex(a, pnts) ifFalse(
						reflx remove(a)
						testOutside(a, pnts, reflx) ifTrue(ears prepend(a))
					)
				)

				if(ears contains(b),
					testOutside(b, pnts, reflx) ifFalse(ears remove(b))
				,
					testReflex(b, pnts) ifFalse(
						reflx remove(b)
						testOutside(b, pnts, reflx) ifTrue(ears append(b))
					)
				)
			)
			pols append(verts)
		)
	)

	FACE := method(sc, root,
		pols := List clone
		while(sc isAtEnd not,
			verts := LinkList clone
			nv := sc unpack(sc cursor, "*H") first & 0x3ff; sc advance(2)
			nv repeat(
				verts append(decodeVX(sc))
			)
			triangulate(root, verts, pols)
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
		//st := List clone setSize(root lastLayer pols size)
		st := Map clone
		while(sc isAtEnd not,
			//st atPut(decodeVX(sc), sc unpack(sc cursor, "*H") first)
			p := decodeVX(sc)
			tag := sc unpack(sc cursor, "*H") first asString ; sc advance(2)
			r := st at(tag)
			if(r isNil,
				r = List clone append(p)
			,
				r append(p)
			)
			st atPut(tag, r)
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
		filename ::= nil
		decode := method(sc, root,
			setFilename(decodeS0(sc))
			filename println
			self
		)
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
		index := sc read(4) unpack("*I") first asString
		subcnk := self getSlot(sc read(4))
		r := subcnk clone read(sc, root)
		if(r isNil not, root lastLayer clips atPut(index, r))

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
		decode := method(sc, root, sc unpack("*f") first)
	)
	LUMI := SubChunk clone do (
		//LUMI { intensity[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	SPEC := SubChunk clone do (
		//SPEC { intensity[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	REFL := SubChunk clone do (
		//REFL { intensity[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	TRAN := SubChunk clone do (
		//TRAN { intensity[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	TRNL := SubChunk clone do (
		//TRNL { intensity[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	GLOS := SubChunk clone do (
		//GLOS { glossiness[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	SHRP := SubChunk clone do (
		//SHRP { sharpness[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	BUMP := SubChunk clone do (
		//BUMP { strength[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	SIDE := SubChunk clone do (
		//SIDE { sidedness[U2] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	SMAN := SubChunk clone do (
		//SMAN { max-smoothing-angle[ANG4] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	RFOP := SubChunk clone do (
		//RFOP { reflection-options[U2] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	RIMG := SubChunk clone do (
		//RIMG { image[VX] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	RSAN := SubChunk clone do (
		//RSAN { seam-angle[ANG4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	RBLR := SubChunk clone do (
		//RBLR { blur-percentage[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	RIND := SubChunk clone do (
		//RIND { refractive-index[F4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	TROP := SubChunk clone do (
		//TROP { transparency-options[U2] }
		decode := method(sc, root, sc unpack("*H") first)
	)
	TIMD := SubChunk clone do (
		//TIMG { image[VX] }
		decode := method(sc, root, decodeVX(sc))
	)
	TBLR := SubChunk clone do (
		//TBLR { blur-percentage[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	CLRH := SubChunk clone do (
		//CLRH { color-highlights[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	CLRF := SubChunk clone do (
		//CLRF { color-filter[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f") first)
	)
	ADTR := SubChunk clone do (
		//ADTR { additive[FP4], envelope[VX] }
		decode := method(sc, root, sc unpack("*f") first)
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

	BLOK := SubChunk clone do (
		//BLOK { header[SUB-CHUNK], attributes[SUB-CHUNK] * }
		BlockHeaderSubChunk := SubChunk clone do (
			CHAN := SubChunk clone do (
				//CHAN { texture-channel[ID4] }
			)
			ENAB := SubChunk clone do (
				//ENAB { enable[U2] }
			)
			OPAC := SubChunk clone do (
				//OPAC { type[U2], opacity[FP4], envelope[VX] }
			)
			AXIS := SubChunk clone do (
				//AXIS { displacement-axis[U2] }
			)
			NEGA := SubChunk clone do (
				//NEGA { enable[U2] }
			)

			decode := method(sc, root,
				ordinal := decodeS0(sc)
				ordinal asString println
				while(sc isAtEnd not,
					id := sc read(4)
					id = self getSlot(id)
					id read(sc, root)
				)
			)
		)


		IMAP := BlockHeaderSubChunk clone do (//an image map texture
		)
		PROC := BlockHeaderSubChunk clone do (//a procedural texture
		)
		GRAD := BlockHeaderSubChunk clone do (//a gradient texture
		)
		SHDR := BlockHeaderSubChunk clone do (//a shader plug-in
		)


		//IMAP and PROC attributes
		TMAP := SubChunk clone do (
		)

		//PROC attributes
		AXIS := SubChunk clone do (
			//AXIS { axis[U2] }
		)
		VALU := SubChunk clone do (
			//VALU { value[FP4] # (1, 3) }
		)
		FUNC := SubChunk clone do (
			//FUNC { algorithm-name[S0], data[...] }
		)

		//GRAD attributes
		PNAM := SubChunk clone do (
		)
		INAM := SubChunk clone do (
			//INAM { item-name[S0] }
		)
		GRST := SubChunk clone do (
			//GRST { input-range[FP4] }
		)
		GREN := SubChunk clone do (
			//GREN { input-range[FP4] }
		)
		GRPT := SubChunk clone do (
			//GRPT { repeat-mode[U2] }
		)
		FKEY := SubChunk clone do (
			//FKEY { ( input[FP4], output[FP4] # 4 )* }
		)
		IKEY := SubChunk clone do (
			//IKEY { interpolation[U2] * }
		)

		//IMAP attributes
		IMAG := SubChunk clone do (
			//IMAG { texture-image[VX] }
		)
		PROJ := SubChunk clone do (
			//PROJ { projection-mode[U2] }
		)
		AXIS := SubChunk clone do (
			//AXIS { texture-axis[U2] }
		)
		WRAP := SubChunk clone do (
			//WRAP { width-wrap[U2], height-wrap[U2] }
		)
		WRPW := SubChunk clone do (
			//WRPW { cycles[FP4], envelope[VX] }
		)
		WRPH := SubChunk clone do (
			//WRPH { cycles[FP4], envelope[VX] }
		)
		VMAP := SubChunk clone do (
			//VMAP { txuv-map-name[S0] }
		)
		AAST := SubChunk clone do (
			//AAST { flags[U2], antialising-strength[FP4] }
		)
		PIXB := SubChunk clone do (
			//PIXB { flags[U2] }
		)
		STCK := SubChunk clone do (
			//STCK { on-off[U2], time[FP4] }
		)
		TAMP := SubChunk clone do (
			//TAMP { amplitude[FP4], envelope[VX] }
		)

		decode := method(sc, root,
			while(sc isAtEnd not,
				h := sc read(4)
				h = self getSlot(h)
				h read(sc, root)
			)
		)
	)

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

		Collector setAllocsPerSweep(f size)

		f close

		obj := RawObject clone
		obj lwoFileName := fileName

		form := Lobby getSlot(sc read(4))
		sc size println
		form read(sc, obj)
		sc size println

		obj
	)
)


//fileName := "sponza.lwo"

obj := LwoObject read(System args at(1))

doRelativeFile("OBJ.io")

OBJ with("lwotest.obj") run(obj)
