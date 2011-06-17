/*
*/

OBJ := Object clone do (


	with := method(outputFileName,
		this := self clone
		this outputFileName := outputFileName
		this
	)

	format := method(n,
		n = n asString split(".")
		if(n size < 2,
			return (n first .. ".000000")
		,
			return (n first .. "." .. n at(1) exSlice(0, 6))
		)
	)

	dumpVertex := method(f, points,
		points foreach(p,
			//f write("v " .. p x .. " " .. p y .. " " .. p z .. "\n")
			f write("v " .. format(p x) .. " " .. format(p y) .. " " .. format(p z) .. "\n")
		)
	)

	dumpUVs := method(f, uvMap,
		uvMap foreach(name, txuv,
			f write("#" .. name .. "\n")
			txuv foreach(vx, uv,
				f write("#" .. vx .. "\n")
				f write("vt " .. format(uv at(0)) .. " " .. format(uv at(1)) .. "\n")
			)
		)
	)

	dumpPols := method(f, obj,
		layer := obj lastLayer
		/*layer surfTags foreach(t, l,
			tag := obj tags at(t asNumber)
			f write("g " .. tag .. "\n")
			l foreach(p,
				f write("f")
				pol := layer pols removeAt(p)
				pol foreach(v, f write(" " .. v + 1))
				
				f write("\n")
			)
		)*/

		layer pols foreach(p,
			f write("f")
			p foreach(v, f write(" " .. v + 1))
			f write("\n")
		)
	)

	run := method(rawObj,
		f := File with(outputFileName) remove create openForUpdating

		f write("# Created with lwoParse.io\n")

		f write("mtlib " .. outputFileName asMutable replaceSeq("obj", "mtl") .. "\n")

		dumpVertex(f, rawObj lastLayer points)
		//dumpUVs(f, rawObj lastLayer uvMaps at("vmap"))
		dumpPols(f, rawObj)

		f close
		
	)


)