'Include "jsondecoder.bmx"
'Include "lettermaker.bmx"
'Include "grammar.bmx"
'Include "helpers.bmx"
'Include "globals.bmx"
'Include "mapping.bmx"
'Include "graphics.bmx"

'------------------------------------------------------------------------------------
'------------------------------------------------------------------------------------
'------------------------------------------------------------------------------------
'------------------------------------------------------------------------------------
'------------------------------------------------------------------------------------

Function debugo(txt$)
	If debugging
		Print txt
	EndIf
End Function



Type product
	Field name$
	Field plural$
	Field luxury#
	Field rate#
	
	Method New()
	End Method
	
	Method jsonise:jsonvalue()
		j:jsonobject=New jsonobject
		j.addnewpair("name",jsonstringvalue.Create(name))
		j.addnewpair("plural",jsonstringvalue.Create(plural))
		j.addnewpair("luxury",jsonnumbervalue.Create(luxury))
		Return j
	End Method
	
	Function Load:product(j:jsonobject)
		name$=j.getstringvalue("name")
		plural$=j.getstringvalue("plural")
		luxury#=j.getnumbervalue("luxury")
		Return product.Create(name,plural,luxury)
	End Function
	
	Function Create:product(name$,plural$="",luxury#)
		p:product=New product
		p.name=name
		p.plural=pluralise(name,2,plural)
		p.luxury=luxury
		products.insert(p.name,p)
		products.insert(p.plural,p)
		Return p
	End Function
	
	Function find:product(name$)
		If products.contains(name)
			Return product(products.valueforkey(name))
		EndIf
	End Function
	
	Function all:TList()
		l:TList=New TList
		For p:product = EachIn products.values() 
			If Not l.contains(p) Then l.addlast p
		Next
		Return l
	End Function
End Type

Type resident
	Field money#
	Field warehouse:tmap
	Field home:port
	Field numbought
	Field name$

	Method New()
		warehouse=New tmap
	End Method
	
	Method jsonise:jsonvalue()
		j:jsonobject=New jsonobject
		j.addnewpair("money",jsonnumbervalue.Create(money))
		j.addnewpair("home",jsonstringvalue.Create(home.name))
		j.addnewpair("name",jsonstringvalue.Create(name))
		j.addnewpair("numbought",jsonnumbervalue.Create(numbought))
		wj:jsonobject=New jsonobject
		j.addnewpair("warehouse",wj)
		For p:product=EachIn warehouse.keys()
			wj.addnewpair(p.name,stock(warehouse.valueforkey(p)).jsonise())
		Next
		Return j
	End Method
	
	Method finishload(j:jsonobject)
		name$=j.getstringvalue("name")
		money#=j.getnumbervalue("money")
		numbought=j.getnumbervalue("numbought")
		wj:jsonobject=j.getobjectvalue("warehouse")
		For p:jsonpair=EachIn wj.pairs
			pname$=p.name
			number=wj.getnumbervalue(pname)
			s:stock=getstock(product.find(pname))
			s.amount=number
		Next
	End Method
	
	Method resolvequantity#(quantity$,total#)
		If quantity="all"
			number#=total
		ElseIf validinput("percentage",quantity)
			percentage#=Float(quantity[..Len(quantity)-1])/100.0
			number#=total*percentage
		Else
			number#=Float(quantity)
		EndIf
		Return number#
	End Method
	
	Method getstock:stock(p:product)	
		If Not warehouse.contains(p)
			warehouse.insert(p,New stock)
		EndIf
		Return stock(warehouse.valueforkey(p))
	End Method
	
	Method stockof(p:product)
		s:stock=getstock(p)
		Return stock(warehouse.valueforkey(p)).amount
	End Method
	
	Method numforsale(p:product)
		Return stockof(p)
	End Method
	
	Method movestock(p:product,number)
		s:stock=getstock(p)
		s.amount:+number
	End Method
	
	Method sell(p:product,number,saleprice#)
		movestock(p,-number)
		money:+saleprice
	End Method
	
	Method buy#(p:product , number , cost# , strictnumber = 1 , totalspend# = - 1) 
		If totalspend = - 1
			totalspend = money
		EndIf
		If totalspend > money
			totalspend = money
		EndIf
		numbought=0
		'debugo "buying "+String(number)+" "+p.plural+" for "+String(cost)+" each"
		If money=0 Then Return 0
		vendors:TList=New TList
		available=0
		For r:resident=EachIn home.residents
			rstock=r.numforsale(p)
			If r<>Self And rstock And r.priceof(p)>=0 And (r.priceof(p)<=cost Or cost=-1)
				vendors.addlast r
				available:+rstock
			EndIf
		Next
		
		If available<number And strictnumber=0
			number=available
		EndIf
		
		If available<number Or available=0
			Return -2
		EndIf
				
		Function cmp(o1:Object,o2:Object)
			r1:resident=resident(o1)
			r2:resident=resident(o2)
			If r1.priceof(product(horriblekludge))<r2.priceof(product(horriblekludge))
				Return -1
			Else
				Return 1
			EndIf
		End Function
	
		horriblekludge=p
		vendors.sort(True,cmp)
		
		If number>0
			numleft=number
			cvendors:TList=vendors.copy()
			total#=0
			While numleft>0
				r:resident=resident(cvendors.removefirst())
				rstock=r.numforsale(p)
				If rstock>numleft
					tobuy=numleft
				Else
					tobuy=rstock
				EndIf
				numleft:-tobuy
				total:+tobuy*r.priceof(p)
			Wend
		EndIf
		
		If totalspend<total And number>=0 And money>=0 And strictnumber=1
			Return 0
		Else
			total#=0
			While number<>0 And vendors.count()
				r:resident=resident(vendors.removefirst())
				rstock=r.numforsale(p)
				rprice#=r.priceof(p)
				If rstock<number Or number=-1
					tobuy=rstock
				Else
					tobuy=number
				EndIf
				If tobuy*rprice>totalspend 'And money>=0
					tobuy=totalspend/rprice
					number=tobuy
				EndIf
				rprice:*tobuy
				
				If tobuy>0
					debugo "buying "+String(tobuy)+" "+p.plural+" at "+moneystring(rprice)
					movestock(p,tobuy)
					r.sell(p,tobuy,rprice)
					If number>=0 Then number:-tobuy
					total:+rprice
					If money >= 0 Then money:- rprice
					totalspend:-rprice
					numbought:+tobuy
				EndIf
			Wend
			Return total
		EndIf
	
	End Method
	
	Method priceof#(p:product)
		Return -1
	End Method
	
	Method update()
	End Method		
End Type


Type merchant Extends resident
	Field inbox:TList
	Field sentences:TList
	Field routines:TList
	Field outletters:tmap
	Field prices:tmap
	Field numtosell:tmap
	Field employees:TList
	Field contacts:TList
	Field employer$
	Field contracts:tmap
	Field factories:TList
	Field numsold:tmap
	Field fonts:tmap
	Field success
	
	Method New()
		inbox=New TList
		sentences=New TList
		prices=New tmap
		numtosell=New tmap
		outletters=New tmap
		routines=New TList
		employees=New TList
		contacts=New TList
		contracts=New tmap
		factories=New TList
		numsold=New tmap
		fonts=pickfonts()
	End Method
	
	Method jsonise:jsonvalue()
		j:jsonobject=jsonobject(Super.jsonise())
		j.addnewpair("type",jsonstringvalue.Create("merchant"))
		j.addnewpair("employer",jsonstringvalue.Create(employer))
		lj:jsonarray=New jsonarray
		j.addnewpair("inbox",lj)
		For let:letter=EachIn inbox
			lj.values.addlast let.jsonise()
		Next
		sj:jsonarray=New jsonarray
		j.addnewpair("sentences",sj)
		For s:sentence=EachIn sentences
			sj.values.addlast s.jsonise()
		Next
		rj:jsonarray=New jsonarray
		j.addnewpair("routines",rj)
		For r:routine=EachIn routines
			rj.values.addlast r.jsonise()
		Next
		oj:jsonobject=New jsonobject
		j.addnewpair("outletters",oj)
		For m:merchant=EachIn outletters.keys()
			oj.addnewpair(m.name,letter(outletters.valueforkey(m)).jsonise())
		Next
		pj:jsonobject=New jsonobject
		j.addnewpair("prices",pj)
		For p:product=EachIn prices.keys()
			pj.addnewpair(p.name,stock(prices.valueforkey(p)).jsonise())
		Next
		nj:jsonobject=New jsonobject
		j.addnewpair("numtosell",nj)
		For p:product=EachIn numtosell.keys()
			nj.addnewpair(p.name,stock(numtosell.valueforkey(p)).jsonise())
		Next
		ej:jsonarray=New jsonarray
		j.addnewpair("employees",ej)
		For mname$=EachIn employees
			ej.values.addlast jsonstringvalue.Create(mname)
		Next
		cj:jsonarray=New jsonarray
		j.addnewpair("contacts",ej)
		For mname$=EachIn employees
			cj.values.addlast jsonstringvalue.Create(mname)
		Next
		fj:jsonarray=New jsonarray
		j.addnewpair("factories",fj)
		For fname$=EachIn factories
			fj.values.addlast jsonstringvalue.Create(fname)
		Next
		nj:jsonobject=New jsonobject
		j.addnewpair("numsold",nj)
		For p:product=EachIn numsold.keys()
			nj.addnewpair(p.name,stock(numsold.valueforkey(p)).jsonise())
		Next
		cj:jsonarray=New jsonarray
		j.addnewpair("contracts",cj)
		For l:TList=EachIn contracts.values()
			For c:contract=EachIn l
				cj.values.addlast c.jsonise()
			Next
		Next
		Return j
	End Method
	
	Function Load:merchant(j:jsonobject)
		hname$=j.getstringvalue("home")
		name$=j.getstringvalue("name")
		home:port=port.find(hname)
		m:merchant=merchant.Create(name,home)
		m.finishload(j)
		
		lj:jsonarray=j.getarrayvalue("inbox")
		For letj:jsonobject=EachIn lj.values
			m.inbox.addlast letter.Load(letj)
		Next
		sj:jsonarray=j.getarrayvalue("sentences")
		For so:jsonobject=EachIn sj.values
			m.sentences.addlast sentence.Load(so)
		Next
		rj:jsonarray=j.getarrayvalue("routines")
		For ro:jsonobject=EachIn rj.values
			m.routines.addlast routine.Load(ro)
		Next
		oj:jsonobject=j.getobjectvalue("outletters")
		For p:jsonpair=EachIn oj.pairs
			m.outletters.insert p.name,letter.Load(oj.getobjectvalue(p.name))
		Next
		pj:jsonobject=j.getobjectvalue("prices")
		For p:jsonpair=EachIn pj.pairs
			m.prices.insert product.find(p.name),stock.Load(p.value)
		Next
		nj:jsonobject=j.getobjectvalue("numtosell")
		For p:jsonpair=EachIn nj.pairs
			m.numtosell.insert product.find(p.name),stock.Load(p.value)
		Next
		ej:jsonarray=j.getarrayvalue("employees")
		For sv:jsonstringvalue=EachIn ej.values
			m.employees.addlast sv.txt
		Next
		cj:jsonarray=j.getarrayvalue("contacts")
		For sv:jsonstringvalue=EachIn cj.values
			m.employees.addlast sv.txt
		Next
		m.employer=j.getstringvalue("employer")
		fj:jsonarray=j.getarrayvalue("factories")
		For fv:jsonstringvalue=EachIn fj.values
			m.factories.addlast fv.txt
		Next
		nj:jsonobject=j.getobjectvalue("numsold")
		For p:jsonpair=EachIn nj.pairs
			m.numsold.insert product.find(p.name),stock.Load(p.value)
		Next
		cj:jsonarray=j.getarrayvalue("contracts")
		For cv:jsonvalue=EachIn cj.values
			c:contract=contract.Load(jsonobject(cv))
			m.addcontract c
		Next
		
		Return m
		
	End Function
	
	Function Create:merchant(name$,home:port)
		m:merchant=New merchant
		m.name=name
		m.home=home
		m.money=money
		m.home.residents.addlast m
		merchants.insert(name,m)
		Return m
	End Function
	
	Function find:merchant(mname$)
		If mname="me"
			Return player
		EndIf
		If merchants.contains(mname)
			Return merchant(merchants.valueforkey(mname))
		EndIf
	End Function

	Method findemployee:merchant(mname$)
		If employees.contains(mname)
			Return merchant.find(mname)
		EndIf
	End Method
	
	Method numforsale(p:product)
		If numtosell.contains(p)
			pstock=stockof(p)
			sstock=stock(numtosell.valueforkey(p)).amount
			If pstock<sstock Or sstock=-1
				If pstock<0 Then pstock=0
				Return pstock
			Else
				Return sstock
			EndIf
		Else
			Return 0
		EndIf
	End Method

	Method priceof#(p:product)
		If Not prices.contains(p)
			Return -1
		EndIf
		Return stock(prices.valueforkey(p)).amount
	End Method
	
	Method setprice(p:product,price#,number)
		If prices.contains(p)
			s:stock=stock(prices.valueforkey(p))
		Else
			s:stock=New stock
			prices.insert p,s
		EndIf
		s.amount=price
		If numtosell.contains(p)
			s:stock=stock(numtosell.valueforkey(p))
		Else
			s:stock=New stock
			numtosell.insert p,s
		EndIf
		s.amount=number
	End Method
	
	Method sell(p:product,number,saleprice#)
		Super.sell(p,number,saleprice)
		If employer
			outletter:letter=letterto(merchant.find(employer))
			outletter.addemptysentence "I have sold "+String(number)+" "+p.plural+" for "+moneystring(saleprice),0
		EndIf
		
		If Not numsold.contains(p)
			s:stock=New stock
			numsold.insert p,s
		Else
			s:stock=stock(numsold.valueforkey(p))
		EndIf
		s.amount:+number
	End Method
	
	Method startletter:letter(m:merchant)
		let:letter=letter.Create(m.name,name,m.home.name,fonts)
		inserts:tmap=New tmap
		inserts.insert "home",home.name
		inserts.insert "recipient",m.name
		let.txt=doinserts(gettemplate("letter"),inserts)
		Return let
	End Method
	
	Method finishletter(let:letter,valediction$)
		If Not let.sentences.count()
			let.txt:+"I can't think of anything to say to you.~n"
		EndIf
		'If let.txt[Len(let.txt)-1]<>10
			let.txt:+"~n~n"
		'EndIf
		inserts:tmap=New tmap
		inserts.insert "valediction",valediction
		inserts.insert "name",name
		let.txt:+doinserts(gettemplate("endletter"),inserts)
	End Method
	
	Method deliverletter(let:letter)
		If Not contacts.contains(let.sender)
			contacts.addlast let.sender
		EndIf
		inbox.addlast let
	End Method
	
	Method readletter(let:letter)
		Print let.jsonise().repr()
		outletter:letter=letterto(merchant.find(let.sender))
	
		'report name+" received a letter with "+String(let.sentences.count())+" "+pluralise("sentence",let.sentences.count())
		For s:sentence=EachIn let.sentences
			success=0
			readsentence(s,merchant.find(let.sender),outletter)
		Next
	End Method
	
	Method readsentence(s:sentence,sender:merchant,outletter:letter)
		Print "read sentence"
		Print name+": "+s.repr()
		command$=s.nextparam()
		success=0
		Select command
		Case "after"
			perform()
			readsentence(s,sender,outletter)
		Case "sell","buy"
			quantity$=s.nextparam()
			If quantity="no"
				strictnumber=0
				quantity=s.nextparam()
			Else
				strictnumber=1
			EndIf
			
			If Not quantity
				outletter.addemptysentence "How many should I "+command+"?"
				Return
			EndIf
			pname$=s.nextparam()
			If Not pname
				outletter.addemptysentence "What should I "+command+"?"
				Return
			EndIf
			amount$=s.nextparam()
			p:product=product.find(pname)
			If quantity="all"
				number=-1
			Else
				number=resolvequantity(quantity,stockof(p))
			EndIf

			'cost:*number
			If number=1
				pname=p.name
			Else
				pname=p.plural
			EndIf
			
			ns:sentence=New sentence
			ns.isroutine=s.isroutine
			ns.addparam "orderer",sender.name
			ns.addparam "$",command
			ns.addparam "product",pname
			ns.addparam "number",String(number)
			ns.addparam "amount",amount
			ns.addparam "strictnumber",String(strictnumber)
			ns.txt=s.txt
			sentences.addlast ns
			
		Case "howmany"
			success:+1
			s.nextparam
			s.nextparam
			pname$=s.nextparam()
			If Not pname
				outletter.addemptysentence "I don't understand. How many what?"
				Return
			EndIf
			If pname="money"
				amountheld=money
				amountstring$=moneystring(money)
			Else
				amountheld=stockof(product.find(pname))
				amountstring$=String(amountheld)+" "+pname
			EndIf
			If amountheld=0
				outletter.addemptysentence "I don't have any "+pname
			Else
				outletter.addemptysentence "I have "+amountstring
			EndIf
		Case "whatprice"
			success:+1
			s.nextparam
			pname$=s.nextparam()
			If Not pname
				outletter.addemptysentence "What do you want to know the price of?"
				Return
			EndIf
			p:product=product.find(pname)
			price#=home.priceof(p)
			If price>=0
				If pname=p.name
					outletter.addemptysentence pname+" costs on average "+moneystring(price)+" per unit here."
				Else
					outletter.addemptysentence pname+" cost on average "+moneystring(price)+" per unit here."
				EndIf
			Else
				outletter.addemptysentence "Nobody is trading in "+p.plural+" here"
			EndIf
		Case "send"
			package$=s.nextparam()
			
			If validinput("strictmoney",package)
				gross#=resolveamount(package)
				pname="money"
			Else
				pname$=s.nextparam()
				If pname="money"
					gross#=resolvequantity(package,money)
				Else
					p:product=product.find(pname)
					gross#=resolvequantity(package,stockof(p))
				EndIf
			EndIf
			
			mname$=s.nextparam()

			ns:sentence=New sentence
			ns.isroutine=s.isroutine
			ns.txt=s.txt
			ns.addparam "orderer",sender.name
			ns.addparam "$","send"
			ns.addparam "package",pname
			ns.addparam "gross",String(gross)
			ns.addparam "othermerchant",mname
			sentences.addlast ns


		Case "here"
			success:+1
			package$=s.nextparam()
			If validinput("strictmoney",package)
				gross#=resolveamount(package)
				money:+gross
				outletter.addemptysentence "Thank you for the sum of "+moneystring(gross)+" I received"
			Else
				amount$=s.nextparam()
				p:product=product.find(package)
				movestock(p,Int(amount))
				outletter.addemptysentence "Thank you for the "+amount+" "+package+" I received"
			EndIf
			
		Case "every"
			tperiod$=s.nextparam()
			Select tperiod
			Case "day"
				period=1
			Case "week"
				period=7
			Case "month"
				period=30
			Default
				period=Int(tperiod)
			End Select
			For rs:sentence=EachIn s.split("and")
				rs.nextparam
				Print rs.repr()
				r:routine=routine.Create(rs,sender.name,period)
				routines.addlast r
			Next
			outletter.addemptysentence "I will do as you wish."
		Case "try"
			Print s.repr()
			r:routine=routine.Create(s,sender.name,1,1)
			routines.addlast r
			outletter.addemptysentence "I will try to do what you asked."
		Case "stop"
			success:+1
			routines=New TList
			outletter.addemptysentence "I have stopped what I was doing."
		Case "tell"
			success:+1
			outletter.addemptysentence "Here is a copy of the "+home.name+" journal.~n~n"
			outletter.txt:+home.getnews()
			outletter.txt:+"^ ~qfont~q: ~qhandwriting~q, ~qsize~q: 100^"
		Case "build"
			pname$=s.nextparam()
			p:product=product.find(pname)
			bp:blueprint=blueprint.find(p)
			If money>=bp.cost
				build(bp)
				outletter.addemptysentence "I have built  "+announ(pname)+" factory"',1-s.isroutine
				success:+1
			Else
				outletter.addemptysentence "I do not have enough money to build "+announ(pname)+" factory",1-s.isroutine
			EndIf
		Case "hired"
			employer=sender.name
			sender.employees.addlast name
			outletter.addemptysentence "I have arrived in "+home.name+". I will serve you well here."
		Case "contractbid"
			vendor$=s.nextparam()
			If vendor="my"
				vendor=s.nextparam()
			EndIf
			pname$=s.nextparam()
			p:product=product.find(pname)
			commission#=resolveamount(s.nextparam())
			c:contract=findcontract(p,sender.name)
			If c Or sender.name=employer
				'removecontract c
				c=contract.Create(p,commission,vendor)
				addcontract c
			Else
				outletter.addemptysentence "Sorry, I can not agree to that contract."
			EndIf
		Case "offer"
			number=Int(s.nextparam())
			pname$=s.nextparam()
			p:product=product.find(pname)
			'cprice#=home.priceof(p)*1.5
			commission#=Rnd(.1,.5)
			ns:sentence=New sentence
			ns.txt="I can supply you with "+p.plural+" for "+percentstring(commission)+"% commission"
			ns.addparam "$","contractbid"
			ns.addparam "vendor",name
			ns.addparam "product",p.name
			ns.addparam "commission",percentstring(commission)
			outletter.addsentence ns
		Case "contract"
			pname$=s.nextparam()
			gross#=Float(s.nextparam())
			commission#=resolveamount(s.nextparam())
			p:product=product.find(pname)
			costper#=priceof(p)*(1+commission)
			units=gross/costper
			debugo "fulfilling contract for "+String(units)+" "+p.plural+" from "+sender.name
			money:+gross
			If units>stockof(p)
				tspend#=stockof(p)*costper
			Else
				tspend#=units*costper
			EndIf
			If stockof(p)<units
				tspend#:+buy(p,units-stockof(p),-1,0,gross)
			EndIf
			If units>stockof(p)
				units=stockof(p)
			EndIf
			change#=gross-tspend
			If change>0
				ns:sentence=New sentence
				ns.addparam "orderer",sender.name
				ns.addparam "$","send"
				ns.addparam "package","money"
				ns.addparam "gross",String(change)
				ns.addparam "othermerchant",sender.name
				sentences.addlast ns
			EndIf
			ns:sentence=New sentence
			ns.addparam "orderer",sender.name
			ns.addparam "$","send"
			ns.addparam "package",pname
			ns.addparam "gross",String(units)
			ns.addparam "othermerchant",sender.name
			sentences.addlast ns
		Case "summarise"
			success:+1
			outletter.addemptysentence positionsummary()
		Case "if"
			l:TList=s.split("conjunction")
			action:sentence=sentence(l.removelast())
			test:sentence=sentence.join(l)
			action.nextparam
			test.nextparam
			Print action.repr()
			Print test.repr()
			If evaluate(test)
				readsentence(action,sender,outletter)
			EndIf
		Case "when"
			Local param$[2]
			param=["$","if"]
			s.params.addfirst param
			param=["$","try"]
			s.params.addfirst param
			readsentence s,sender,outletter
		Default
			Print "don't understand"
			Print s.repr()
		End Select
		If s.params.count()
			Select String[](s.params.first())[0]
			Case "and"
				Print "and"
				s.nextparam
				If s.params.count()
					readsentence(s,sender,outletter)
				EndIf
			End Select
		EndIf
	End Method
	
	Method evaluate(s:sentence)
		Print "evaluate "+s.repr()
		l:TList=s.split("conjunction","or")
		If l.count()=1
			l=s.split("conjunction","and")
			If l.count()>1
				go=1
				For s:sentence=EachIn l
					s.nextparam
					go:*evaluate(s)
				Next
				Return go
			EndIf
		Else
			go=0
			For s:sentence=EachIn l
				s.nextparam
				go:+evaluate(s)
			Next
			Return go
		EndIf

		expression$=s.nextparam()
		Select expression
		Case "priceof"
			pname$=s.nextparam()
			p:product=product.find(pname)
			price#=home.priceof(p)
			comp$=s.nextparam()
			num1#=resolveamount(s.nextparam())
			Select comp
			Case "lessthan"
				If price<num1
					Return 1
				Else
					Return 0
				EndIf
			Case "morethan"
				If price>num1
					Return 1
				Else
					Return 0
				EndIf
			Case "between"
				num2#=resolveamount(s.nextparam())
				If (price>=num1 And price<=num2) Or (price>=num2 And price<=num1)
					Return 1
				Else
					Return 0
				EndIf
			End Select
		Case "youhave"
			comp$=s.nextparam()
			Select comp
			Case "atleast"
				numstr$=s.nextparam()
				amount#=resolveamount(numstr)
				If numstr[0]=36
					compamount#=money
				Else
					pname$=s.nextparam()
					p:product=product.find(pname)
					compamount#=stockof(p)
				EndIf
				If compamount>=amount
					Return 1
				Else
					Return 0
				EndIf
			Case "morethan"
				numstr$=s.nextparam()
				amount#=resolveamount(numstr)
				If numstr[0]=36
					compamount#=money
				Else
					pname$=s.nextparam()
					p:product=product.find(pname)
					compamount#=stockof(p)
				EndIf
				If compamount>amount
					Return 1
				Else
					Return 0
				EndIf
			Case "fewerthan"
				numstr$=s.nextparam()
				amount#=resolveamount(numstr)
				If numstr[0]=36
					compamount#=money
				Else
					pname$=s.nextparam()
					p:product=product.find(pname)
					compamount#=stockof(p)
				EndIf
				Print "compamount: "+String(compamount)
				Print "amount: "+String(amount)
				If compamount<amount
					Return 1
				Else
					Return 0
				EndIf
			Case "no"
				pname$=s.nextparam()
				If pname$="money"
					amount#=money
				Else
					p:product=product.find(pname)
					amount#=stockof(p)
				EndIf
				If amount<=0
					Return 1
				Else
					Return 0
				EndIf
			End Select
		Case "forsale"
			isare$=s.nextparam()
			If isare="any"
				pname$=s.nextparam()
				number=1
			Else
				number=resolveamount(s.nextparam())
				pname$=s.nextparam()
			EndIf
			p:product=product.find(pname)
			If home.numforsale(p)>=number
				Return 1
			Else
				Return 0
			EndIf
		Case "monthis"
			mname$=s.nextparam()
			Print month
			Print resolvemonth(mname)
			If month=resolvemonth(mname)
				Return 1
			Else
				Return 0
			EndIf
		End Select
	End Method
	
	
	Method positionsummary$()
		inserts:tmap=New tmap
		inserts.insert "money",moneystring(money)
		fown$="I own "
		lown:TList=New TList
		col1$=""
		col2$=""
		col3$=""
		For p:product=EachIn product.all()
			n=0
			For fname$=EachIn factories
				f:factory=factory(home.findresident(fname))
				If f.output=p
					n:+1
				EndIf
			Next
			If n>0
				lown.addlast String(n)+" "+pluralise("factory",n,"factories")+" producing "+p.plural
			EndIf
			
			If stockof(p)>0 Or priceof(p)>0
				If col1
					col1:+"~n~n"
					col2:+"~n~n"
					col3:+"~n~n"
				EndIf
				col1:+p.plural
				col2:+String(stockof(p))
				col3:+moneystring(priceof(p))
			EndIf
		Next
		If lown.count()
			fown:+prettylist(lown)
		Else
			fown="I don't own any factories"
		EndIf
		inserts.insert "factoryown",prettysentence(fown)
		inserts.insert "col1",col1
		inserts.insert "col2",col2
		inserts.insert "col3",col3
		
		Return doinserts(gettemplate("summary"),inserts)
	End Method
	
	Method build(bp:blueprint)
		money:-bp.cost
		f:factory=factory.Create(home,bp.output,bp.rates,name)
		factories.addlast f.name
		home.addnews name+" built a "+bp.output.name+" factory"
	End Method
		
	Method perform()
		Print "perform"
		For s:sentence=EachIn sentences
			performsentence(s)
		Next
		sentences=New TList
	End Method
	
	Method doroutine(r:routine)
		Print "performing routine "+r.s.txt
		r.nextrun:-1
		If r.nextrun<=0
			orderer:merchant=merchant.find(r.orderer)
			outletter:letter=letterto(orderer)
			s:sentence=r.s.copy()
			Print s.repr()
			s.isroutine=1
			success=0
			readsentence(s,orderer,outletter)
			perform
			r.nextrun=r.period
			Print "success: "+String(success)
			If success
				routines.remove r
				Print "ROUTINE REMOVED"
				outletter.numinfo:+1
			EndIf
		EndIf
	End Method
	
	Method performsentence(s:sentence)
		orderer$=s.nextparam()
		outletter:letter=letterto(merchant.find(orderer))
		'debugo name+": "+s.repr()
		command$=s.nextparam()
		Select command
		Case "sell","buy"
			pname$=s.nextparam()
			
			number=Int(s.nextparam())
			amount$=s.nextparam()
			strictnumber=Int(s.nextparam())
			p:product=product.find(pname)
			
			If amount="goingprice"
				If command="buy"
					cost#=-1
				Else
					cost=home.priceof(p)
				EndIf
			ElseIf validinput("percentage",amount)
				cost#=resolvequantity(amount,home.priceof(p))
				'report amount+" of cost of "+pname+" ("+moneystring(home.priceof(p))+") is "+String(cost)
			Else
				cost#=resolveamount(amount)
			EndIf

			If number=0 Or (number=-1 And stockof(p)=0 And command="sell")
				outletter.addemptysentence "I did not "+command+" any "+p.plural,1-s.isroutine
				Return
			EndIf
			
			total#=cost*number
			If command="sell"
				Rem
				Select sell(p,number,cost)
				Case 1 'success
					outletter.addemptysentence "I have sold "+String(number)+" "+pname+" for "+moneystring(total)+" total"
				Case 0 'not enough stock
					outletter.addemptysentence "I don't have enough "+p.plural+" to sell."
				Case -1 'price too high
					outletter.addemptysentence "I can not sell my "+pluralise(p.name,number,p.plural)+" for that much."
				End Select
				EndRem
				
				setprice(p,cost,number)
				If number=-1
					numtext$="all of my"
				Else
					numtext$=String(number)
				EndIf
				outletter.addemptysentence "I am selling "+String(numtext)+" "+p.plural+" for "+moneystring(cost)+" each",1-s.isroutine
				success:+1
			Else
				total#=buy(p,number,cost,strictnumber)
				If total>0
					outletter.addemptysentence "I have bought "+String(numbought)+" "+pname+" for "+moneystring(total)+" total",1-s.isroutine
					success:+1
				Else
					Select total
					Case 0 'not enough money
						If number=-1
							outletter.addemptysentence "I don't have enough money to buy any "+pluralise(p.name,number,p.plural),1-s.isroutine
						Else
							outletter.addemptysentence "I don't have enough money to buy "+String(number)+" "+pluralise(p.name,number,p.plural),1-s.isroutine
						EndIf
					Case -1 'price too low
						outletter.addemptysentence "I can not buy "+p.plural+" that cheaply.",1-s.isroutine
					Case -2 'not enough stock
						outletter.addemptysentence "The market does not have enough "+p.plural+" available to buy.",1-s.isroutine
					End Select
				EndIf
			EndIf
			
		Case "send"
			pname$=s.nextparam()
			gross#=Float(s.nextparam())
			mname$=s.nextparam()
			m:merchant=merchant.find(mname)
			
			
			If gross=0
				outletter.addemptysentence "I have not sent any "+pname+" to "+mname,1-s.isroutine
			Else
				ns:sentence=New sentence
				ns.addparam "$","here"
				If pname="money"
					grossstr$=moneystring(gross)
					If gross<=money
						money:-gross
						ns.txt="Find enclosed "+grossstr
						ns.addparam "package",grossstr
					Else
						outletter.addemptysentence "I can not send that much money -- "+grossstr+" -- to "+mname,1-s.isroutine
					EndIf
				Else
					p:product=product.find(pname)
					If stockof(p)>=gross
						grossstr$=String(Int(gross))
						ns.txt="Here "+pluralise("is",gross,"are")+" "+grossstr+" "+pluralise(p.name,gross,p.plural)
						ns.addparam "package",pname
						ns.addparam "product",grossstr
						movestock(p,-gross)
						success:+1
					Else
						outletter.addemptysentence "I do not have enough "+p.plural+" to send to "+mname,1-s.isroutine
					EndIf
				EndIf
				If ns.txt
					If m.name=outletter.recipient
						outletter.addsentence ns
					Else
						If pname="money"
							outletter.addemptysentence "I have sent "+grossstr+" to "+mname,1-s.isroutine
						Else
							outletter.addemptysentence "I have sent "+grossstr+" "+pname+" to "+mname,1-s.isroutine
						EndIf
						sletter:letter=letterto(m)
						sletter.addsentence ns
					EndIf
				EndIf
			EndIf
		End Select
	End Method
		
	Method letterto:letter(m:merchant)
		If Not outletters.contains(m)
			outletters.insert(m,startletter(m))
		EndIf
		let:letter=letter(outletters.valueforkey(m))
		If Not contacts.contains(m.name)
			contacts.addlast m.name
			let.addemptysentence "Allow me to introduce myself. I am "+name+", a merchant of "+home.name+". I hope we can do profitable business together."
		EndIf
		Return let
	End Method
		
	Method update()
		debugging=1
		debugo "=="+name
		debugo "I have "+moneystring(money)
		debugging=0
		finish
		
		outletters=New tmap
		For let:letter=EachIn inbox
			readletter(let)
		Next
		inbox=New TList
		
		perform
		For r:routine=EachIn routines
			doroutine(r)
		Next
		
		For l:TList=EachIn contracts.values()
			For c:contract=EachIn l
				If c.daysleft>0
					c.daysleft:-1
					If c.daysleft=0
						'removecontract c
					EndIf
				EndIf
			Next
		Next
		
		manage
		
		l:TList=New TList
		debugging=1
		For p:product=EachIn product.all()
			If Not l.contains(p)
				If stockof(p)>0
					If p.luxury=0
						setprice(p,home.priceof(p)*.99,-1)
					EndIf
					debugo "selling "+String(stockof(p))+" "+p.plural+" for "+moneystring(priceof(p))+" each"
				EndIf
				l.addlast p
			EndIf
		Next
		debugging=0
	End Method
	
	Method manage()
		debugo "managing!"
		priceperlux#=0
		numps=0
		For p:product=EachIn products.values()
			If p.luxury>0 And home.numforsale(p)>0
				numps:+1
				priceperlux:+home.priceof(p)/p.luxury
			EndIf
		Next
		If numps
			priceperlux:/numps
		Else
			priceperlux=-1
		EndIf
		
		For fname$=EachIn factories
			f:factory=factory(home.findresident(fname))
			If f.output.name="beef" debugging=1
			costper#=0
			minunits=-1
			unbuyable:TList=New TList
			For ip:product=EachIn f.rates.keys()
				rate#=stock(f.rates.valueforkey(ip)).amount
				costper:+rate*home.priceof(ip)
				saleunits=home.numforsale(ip)
				units=(saleunits+f.stockof(ip)+stockof(ip))/rate
				If units 
					If units<minunits Or minunits=-1
						minunits=units
					EndIf
				Else
					unbuyable.addlast ip
				EndIf
				
			Next
			
			
			sold:stock=stock(numsold.valueforkey(f.output))



			If sold
				debugo "sold "+String(sold.amount)+" "+f.output.plural+" yesterday"
				samount=sold.amount*2+3
				sold.amount=0
			Else
				samount=0
			EndIf
			samount:-stockof(f.output)
			If samount<0 Then samount=0
			If stockof(f.output)=0 And samount=0
				nsamount=20
			Else
				nsamount=samount
			EndIf
			If minunits>nsamount Or minunits<0
				minunits=nsamount
			EndIf
			
			debugo minunits
			
			If unbuyable.count()
				minunits:*2
				For ip:product=EachIn unbuyable
					rate#=stock(f.rates.valueforkey(ip)).amount
					tobuy=minunits*3*rate-(f.stockof(ip)+stockof(ip))
					
					l:TList=getcontracts(ip).copy()
					For c:contract=EachIn l
						If c.price=-1 Then l.remove c
					Next
					
					Function ccmp(o1:Object,o2:Object)
						c1:contract=contract(o1)
						c2:contract=contract(o2)
						If c1.price<c2.price
							Return -1
						Else
							Return 1
						EndIf
					End Function
					
					l.sort(True,ccmp)
					bcostper#=0
					'For c:contract=EachIn l
					If l.count()
						c:contract=contract(l.first())
						Print "contract to buy "+ip.name+" from "+c.vendor+" for "+percentstring(c.price)
						m:merchant=merchant.find(c.vendor)
						ucostper#=m.priceof(ip)*(1+c.price)
						'If m.numforsale(ip)>tobuy
							mbuy=tobuy
						'Else
							'mbuy=m.numforsale(ip)
						'EndIf
						tcost#=mbuy*ucostper
						If tcost>money
							mbuy=Int(money/ucostper)
							tcost=mbuy*ucostper
						EndIf
						If mbuy>0
							let:letter=letterto(m)
							ns:sentence=New sentence
							ns.txt="As agreed in our contract, here is "+moneystring(tcost)+" for "+ip.plural
							ns.addparam "$","contract"
							ns.addparam "product",ip.name
							ns.addparam "amount",String(tcost)
							ns.addparam "commission",String(c.price)
							let.addsentence ns
							tobuy:-mbuy
							money:-tcost
							costper:+rate*ucostper
						EndIf
					EndIf
					'Next
					If tobuy
						stockists:TList=findstockist(ip,home)
						'For m:merchant=EachIn stockists
						'	If m.employer
						'		em:merchant=merchant.find(m.employer)
						'		If Not stockists.contains(em)
						'			stockists.addlast em
						'		EndIf
						'		stockists.remove m
						'	EndIf
						'Next
						For c:contract=EachIn getcontracts(ip)
							stockists.remove merchant.find(c.vendor)
						Next
						For m:merchant=EachIn stockists
							outletter:letter=letterto(m)
							ns:sentence=New sentence
							ns.txt="Can you supply me with "+String(tobuy)+" "+ip.plural+" per day?"
							ns.addparam "$","offer"
							ns.addparam "number",String(tobuy)
							ns.addparam "product",ip.name
							outletter.addsentence ns
							c:contract=contract.Create(ip,-1,m.name)
							addcontract c
						Next
					EndIf
				Next			
			EndIf	
					
			For ip:product=EachIn f.rates.keys()
				rate#=stock(f.rates.valueforkey(ip)).amount
				tobuy=minunits*rate-(f.stockof(ip)+stockof(ip))
				If tobuy>0
					buy(ip,tobuy,-1,0,tobuy*home.priceof(ip)*2)
				EndIf
				setprice(ip,home.priceof(ip)*1.5,-1)
			Next

			oprice#=priceof(f.output)
			If f.output.luxury>0 And priceperlux>0
				luxprice#=priceperlux*f.output.luxury
			Else
				luxprice#=oprice
			EndIf
			debugo "oprice:~t~t"+String(oprice)
			debugo "costper:~t"+String(costper)
			debugo "luxprice:~t"+String(luxprice)
			If oprice=-1
				oprice=costper
			Else
				If oprice<costper
					oprice:+(costper-oprice)*.1
					debugo "cp"+String(oprice)
				EndIf
				If oprice<luxprice
					oprice:+(luxprice-oprice)*.1
					debugo "lp"+String(oprice)
				EndIf
				If samount
					oprice:+.0001*samount*(f.output.luxury+1)
					debugo "sell"+String(oprice)
				Else
					oprice:*.9
					debugo "ns"+String(oprice)
				EndIf
			EndIf
			debugo "newprice:~t"+String(oprice)

			setprice(f.output,oprice,-1)
			Print "selling "+String(stockof(f.output))+" "+f.output.plural+" at "+moneystring(oprice)
		Next
		
		For fa:farmer=EachIn home.residents
			If money>50
				maxprice#=fa.startprice*.5
				If fa.priceof(fa.p)<maxprice And fa.stockof(fa.p)
					buy(fa.p,10,maxprice,0,50)
					setprice(fa.p,maxprice,-1)
				EndIf
			EndIf
		Next
	End Method
	
	Method getcontracts:TList(p:product)
		If Not contracts.contains(p)
			contracts.insert p,New TList
		EndIf
		Return TList(contracts.valueforkey(p))
	End Method
	
	Method addcontract(c:contract)
		getcontracts(c.p).addlast c
		debugo "adding a contract for "+c.p.plural+" at "+moneystring(c.price)+" from "+c.vendor
	End Method
	
	Method removecontract(c:contract)
		TList(contracts.valueforkey(c.p)).remove c
	End Method
	
	Method findcontract:contract(p:product,mname$)
		For c:contract=EachIn getcontracts(p)
			If c.vendor=mname
				Return c
			EndIf
		Next
	End Method
	
	Method findstockist:TList(p:product,h:port,checked:TList=Null)
		'DebugStop
		debugging=1
		If Not checked
			checked=New TList
		EndIf
		If checked.contains(h)
			Return New TList
		EndIf
		checked.addlast h
		debugo "checking "+h.name+" for "+p.plural
		l:TList=New TList
		
		For m:merchant=EachIn h.residents
			If m<>Self
				debugo "  "+m.name+" ("+String(m.priceof(p))+")"
				If m.priceof(p)>0
					l.addlast m
				EndIf
			EndIf
		Next
		
		For hname$=EachIn h.distances.keys()
			h2:port=port.find(hname)
			For m:merchant=EachIn findstockist(p,h2,checked)
				l.addlast m
			Next
		Next
		debugging=0
		Return l
	End Method
	
	Method finish()
		For let:letter=EachIn outletters.values()
			If let.numinfo
				finishletter(let,"faithfully")
				m:merchant=merchant.find(let.recipient)
				If m.home=home
					m.deliverletter let
				Else
					home.sendletter(let)
				EndIf
			EndIf
		Next
	End Method
	
End Type

Type routine
	Field orderer$
	Field s:sentence
	Field period
	Field nextrun
	Field tryonce
	
	Method jsonise:jsonvalue()
		j:jsonobject=New jsonobject
		j.addnewpair("orderer",jsonstringvalue.Create(orderer))
		j.addnewpair("sentence",s.jsonise())
		j.addnewpair("period",jsonnumbervalue.Create(period))
		j.addnewpair("nextrun",jsonnumbervalue.Create(nextrun))
		j.addnewpair("tryonce",jsonnumbervalue.Create(tryonce))
		Return j
	End Method
	
	Function Load:routine(j:jsonobject)
		s:sentence=sentence.Load(j.getobjectvalue("sentence"))
		orderer$=j.getstringvalue("orderer")
		period=j.getnumbervalue("period")
		tryonce=j.getnumbervalue("tryonce")
		r:routine=routine.Create(s,orderer,period,tryonce)
		r.nextrun=j.getnumbervalue("nextrun")
		Return r
	End Function
	
	Function Create:routine(s:sentence,orderer$,period,tryonce=0)
		r:routine=New routine
		r.s=s
		r.orderer=orderer
		r.period=period
		r.tryonce=tryonce
		Return r
	End Function
End Type

Type contract
	Field p:product
	Field price#
	Field daysleft
	Field vendor$
	
	Method jsonise:jsonvalue()
		j:jsonobject=New jsonobject
		j.addnewpair("product",jsonstringvalue.Create(p.name))
		j.addnewpair("price",jsonnumbervalue.Create(price))
		j.addnewpair("daysleft",jsonnumbervalue.Create(daysleft))
		j.addnewpair("vendor",jsonstringvalue.Create(vendor))
		Return j
	End Method
	
	Function Load:contract(j:jsonobject)
		pname$=j.getstringvalue("product")
		p:product=product.find(pname)
		price#=j.getnumbervalue("price")
		vendor$=j.getstringvalue("vendor")
		c:contract=contract.Create(p,price,vendor)
		c.daysleft=j.getnumbervalue("daysleft")
		Return c
	End Function
	
	Function Create:contract(p:product,price#,vendor$)
		c:contract=New contract
		c.p=p
		c.price=price
		c.daysleft=30
		c.vendor=vendor
		Return c
	End Function
End Type

Type farmer Extends resident
	Field p:product
	Field harvestmonth
	Field harvest
	Field startprice#
	Field price#,vprice#
	Field selling
	Field numsold
	
	Method jsonise:jsonvalue()
		j:jsonobject=jsonobject(Super.jsonise())
		j.addnewpair("type",jsonstringvalue.Create("farmer"))
		j.addnewpair("product",jsonstringvalue.Create(p.name))
		j.addnewpair("harvest",jsonnumbervalue.Create(harvest))
		j.addnewpair("harvestmonth",jsonnumbervalue.Create(harvestmonth))
		j.addnewpair("startprice",jsonnumbervalue.Create(startprice))
		j.addnewpair("price",jsonnumbervalue.Create(price))
		j.addnewpair("vprice",jsonnumbervalue.Create(vprice))
		j.addnewpair("selling",jsonnumbervalue.Create(selling))
		Return j
	End Method
	
	Function Load:farmer(j:jsonobject)
		home:port=port.find(j.getstringvalue("home"))
		p:product=product.find(j.getstringvalue("product"))
		startprice#=j.getnumbervalue("startprice")
		harvestmonth=j.getnumbervalue("harvestmonth")
		harvest=j.getnumbervalue("harvest")
		f:farmer=farmer.Create(home,p,startprice,harvestmonth,harvest)
		f.price#=j.getnumbervalue("price")
		f.vprice#=j.getnumbervalue("vprice")
		f.selling=j.getnumbervalue("selling")
		f.finishload(j)
		Return f
	End Function
	
	Function Create:farmer(home:port,p:product,startprice#,harvestmonth,harvest)
		f:farmer=New farmer
		f.home=home
		f.home.residents.addlast f
		f.startprice=startprice
		f.harvestmonth=harvestmonth
		f.harvest=harvest
		f.p=p
		f.name=p.name+" farmer ("+monthname[harvestmonth]+")"
		
		Return f
	End Function
	
	Method update()
		debugo "=="+p.name+" farmer"
		debugo "farmer has "+String(stockof(p))+" "+p.plural
		debugo "farmer has "+moneystring(money)
		If month=harvestmonth And day=1
			total=Rnd(.5,1.5)*harvest
			movestock(p,-stockof(p))
			movestock(p,total)
			price=startprice
			home.addnews "It is harvest time in "+home.name+"! "+String(total)+" "+p.plural+" arrive"
		EndIf

		pstock=stockof(p)
		selling=poisson(harvest*.1)
		If selling>pstock Then selling=pstock
		
		vprice:+(startprice-price)*.1
		If numsold>0
			vprice:+numsold*.01
		Else
			If vprice>0
				vprice=0
			EndIf
			vprice:-startprice*.01
		EndIf
		vprice:*.8
		price:+vprice
		If price<.02 Then price=.02
		
		numsold=0
		debugo "I am selling "+String(selling)+" of my stock of "+String(stockof(p))+" for "+moneystring(price)+" per unit"
		
		'doesn't belong to anyone, give money back to populace
		home.populace.money:+money
		money=0
	End Method
	
	Method numforsale(bp:product)
		If bp=p
			Return selling
		Else
			Return 0
		EndIf
	End Method
	
		
	
	Method sell(p:product,number,saleprice#)
		Super.sell(p,number,saleprice)
		'price:*(1.02)^number
		numsold:+number
	End Method
	
	Method priceof#(bp:product)
		If bp=p
			Return price
		Else
			Return -1
		EndIf
	End Method
End Type

Type blueprint
	Field output:product
	Field cost#
	Field rates:tmap
	
	Method New()
	End Method
	
	Method jsonise:jsonvalue()
		j:jsonobject=New jsonobject
		j.addnewpair("output",jsonstringvalue.Create(output.name))
		j.addnewpair("cost",jsonnumbervalue.Create(cost))
		rj:jsonobject=New jsonobject
		j.addnewpair("rates",rj)
		For p:product=EachIn rates.keys()
			s:stock=stock(rates.valueforkey(p))
			rj.addnewpair(p.name,jsonnumbervalue.Create(s.amount))
		Next
		Return j
	End Method
	
	Function Load:blueprint(j:jsonobject)
		boutput:product=product.find(j.getstringvalue("output"))
		cost#=j.getnumbervalue("cost")
		rj:jsonobject=j.getobjectvalue("rates")
		rates:tmap=New tmap
		For p:jsonpair=EachIn rj.pairs
			rates.insert product.find(p.name),stock.Load(p.value)
		Next
		bp:blueprint=blueprint.Create(boutput,cost,rates)
		Return bp
	End Function
	
	Function Create:blueprint(output:product,cost#,rates:tmap)
		bp:blueprint=New blueprint
		bp.output=output
		bp.cost=cost
		bp.rates=rates
		blueprints.insert(output,bp)
		Return bp
	End Function
	
	Function find:blueprint(output:product)
		If blueprints.contains(output)
			Return blueprint(blueprints.valueforkey(output))
		EndIf
	End Function
End Type

Type factory Extends resident
	Field output:product
	Field rates:tmap,price#
	Field owner$
	
	Method New()
		price=.3
		money=-1
	End Method
	
	Method jsonise:jsonvalue()
		j:jsonobject=jsonobject(Super.jsonise())
		j.addnewpair("type",jsonstringvalue.Create("factory"))
		j.addnewpair("output",jsonstringvalue.Create(output.name))
		rj:jsonobject=New jsonobject
		j.addnewpair("rates",rj)
		For p:product=EachIn rates.keys()
			rj.addnewpair(p.name,stock(rates.valueforkey(p)).jsonise())
		Next
		j.addnewpair("price",jsonnumbervalue.Create(price))
		j.addnewpair("owner",jsonstringvalue.Create(owner))
		Return j
	End Method
	
	Function Load:factory(j:jsonobject)
		home:port=port.find(j.getstringvalue("home"))
		foutput:product=product.find(j.getstringvalue("output"))
		rj:jsonobject=j.getobjectvalue("rates")
		rates:tmap=New tmap
		For p:jsonpair=EachIn rj.pairs
			rates.insert product.find(p.name),stock.Load(p.value)
		Next
		owner$=j.getstringvalue("owner")
		f:factory=factory.Create(home,foutput,rates,owner)
		f.finishload(j)
		f.price#=j.getnumbervalue("price")
		Return f
	End Function
	
	Function Create:factory(home:port,output:product,rates:tmap,owner$)
		f:factory=New factory
		f.home=home
		f.home.residents.addlast f
		f.output=output
		f.rates=rates
		f.owner=owner
		f.name=owner+"'s "+output.name+" factory"
		Return f
	End Function
	
	Method priceof#(bp:product)
		Return -1
	'	If bp=output
	'		Return price
	'	Else
	'		Return -1
	'	EndIf
	End Method
	
	Method numforsale(bp:product)
		Return 0
	'	If bp=output
	'		Return stockof(bp)
	'	Else
	'		Return 0
	'	EndIf
	End Method
	
	Method sell(p:product,number,saleprice#)
		Super.sell(p,number,saleprice)
		price:*(1.02)^number
	End Method

	'Method movestock(p:product,number)
	'	debugo "moving "+String(number)+" "+p.plural
	'	Super.movestock(p,number)
	'End Method
	
	Method update()
		debugo "=="+output.name+" factory"
		
		debugo "factory has "+moneystring(money)
		debugo "factory has "+String(stockof(output))+" "+output.plural
		
		If stockof(output)
			price:*Rnd(.9,.99)
		EndIf

		minwork=-1
		For ip:product=EachIn rates.keys()
			debugo "factory has "+String(stockof(ip))+" "+ip.plural
			If owner
				m:merchant=merchant.find(owner)
				instock=m.stockof(ip)
				If instock>0
					debugo "moving "+String(instock)+" "+ip.plural+" from "+m.name
					m.movestock(ip,-instock)
					movestock(ip,instock)
				EndIf
			Else
				cost#=buy(ip,-1,-1)
			EndIf
			rate#=stock(rates.valueforkey(ip)).amount
			number=stockof(ip)/rate
			If number<minwork Or minwork=-1
				minwork=number
			EndIf
		Next
		
		If minwork>0
			debugo "factory making "+String(minwork)+" "+output.plural
			For ip:product=EachIn rates.keys()
				rate#=stock(rates.valueforkey(ip)).amount
				movestock(ip,-minwork*rate)
				debugo "using "+String(minwork*rate)+" "+ip.plural
				debugo stockof(ip)
			Next
			movestock(output,minwork)
		EndIf
		
		debugo "I am owned by "+owner
		If owner
			If stockof(output)
				m:merchant=merchant.find(owner)
				outstock=stockof(output)
				debugo "moving "+String(outstock)+" "+output.plural+" to "+m.name
				movestock(output,-outstock)
				m.movestock(output,outstock)
			EndIf
		EndIf
		
	End Method
End Type

Type pleb Extends resident
	Field population
	Field omoney
	
	Method jsonise:jsonvalue()
		j:jsonobject=jsonobject(Super.jsonise())
		j.addnewpair("type",jsonstringvalue.Create("pleb"))
		j.addnewpair("population",jsonnumbervalue.Create(population))
		Return j
	End Method
	
	Function Load:pleb(j:jsonobject)
		home:port=port.find(j.getstringvalue("home"))
		population=j.getnumbervalue("population")
		p:pleb=pleb.Create(home,population)
		p.finishload(j)
		Return p
	End Function
	
	Function Create:pleb(home:port,population)
		p:pleb=New pleb
		p.home=home
		p.home.residents.addlast p
		p.population=population
		Return p
	End Function
	
	Method numforsale(p:product)
		Return 0
	End Method
	
	Method update()
		debugo "==pleb"
		debugo "we have "+moneystring(money)

		If population>100
			inflation#=population*.005
			money:+inflation
		EndIf

		ale:product = product.find("ale") 

		beef:product=product.find("beef")
		beefneeded=population*.008
		beefstock=stockof(beef)
		debugo "we have "+String(beefstock)+" beef in stock"
		If beefstock>=beefneeded
			movestock(beef,-beefneeded)
			debugo "ate "+String(beefneeded)+" beef"
			beefneeded=0
		Else
			totalspend# = .03 * money
			debugo "spending at most "+moneystring(totalspend)+" on beef"
			buy(beef,beefneeded-beefstock,-1,0,totalspend)
			beefstock=stockof(beef)
			If beefstock>=beefneeded
				movestock(beef,-beefneeded)
				debugo "ate "+String(beefneeded)+" beef"
				beefneeded=0
			Else
				beefneeded=beefstock-beefneeded
				movestock(beef,-beefstock)
			EndIf
		EndIf
		If beefneeded<0 'starving!
			starving=poisson(-beefneeded*.05)
			population:-starving
			If starving>0
				debugo String(starving)+" "+pluralise("person",starving,"people")+" starved for want of beef."
				'home.addnews String(starving)+" "+pluralise("person",starving,"people")+" starved for want of beef."
			EndIf
		Else
			buy(ale,population*.01,-1,0,money*.02)
			aleeffect# = Sqr(stockof(ale) / Float(population) ) 
			movestock(ale,-stockof(ale))
			born=poisson((1+aleeffect)*population/2000.0)
			population:+born
			If born>0
				debugo String(born)+" "+pluralise("baby",born,"babies")+" born"
			EndIf
			buyluxuries
		EndIf
		
		enterprise
	End Method
	
	Method buyluxuries()
		debugo "buying luxuries"
		l:TList=New TList
		tstock=0
		For p:product=EachIn products.values()
			If Not l.contains(p) 
				If home.priceof(p)>0 And p.luxury>0
					l.addlast(p)
					debugo " "+p.name
					p.rate=p.luxury/home.priceof(p)
					pstock=stockof(p)
					movestock(p,-pstock*.1)
					tstock:+stockof(p)
				EndIf
			EndIf
		Next
		For p:product=EachIn l
			pstock=stockof(p)
			If pstock>0
				p.rate:*tstock/stockof(p)
			EndIf
		Next
		
		Function luxcmp(o1:Object,o2:Object)
			p1:product=product(o1)
			p2:product=product(o2)
			If p1.rate<p2.rate
				Return 1
			Else
				Return -1
			EndIf
		End Function
	
		l.sort(True,luxcmp)
		'totalspend#=money*.05
		totalspend#=(money-omoney)*.7+money*.05
		If totalspend>0 And omoney>100
			debugo "spending at most "+moneystring(totalspend)+" on luxuries"
			For p:product=EachIn l
				debugo "  "+p.name+" : "+String(p.rate)
				spent#=buy(p,-1,-1,0,totalspend)
				totalspend:-spent
			Next
		EndIf
		omoney=money
	End Method
	
	Method enterprise()
		l:TList=New TList
		For bp:blueprint=EachIn blueprints.values()
			If Not home.numforsale(bp.output)
				l.addlast bp
			EndIf
		Next
		
		Function bpcmp(o1:Object,o2:Object)
			bp1:blueprint=blueprint(o1)
			bp2:blueprint=blueprint(o2)
			If bp1.output.luxury<bp2.output.luxury
				Return -1
			Else
				Return 1
			EndIf
		End Function
		
		l.sort(True,bpcmp)
		i=0
		For bp:blueprint=EachIn l
			i:+1
			tcost#=bp.cost+150
			go=1
			For f:factory=EachIn home.residents
				If f.output=bp.output
					go:+1
					Exit
				EndIf
			Next
			p#=go*go*Sqr(tcost/money)
			If bp.output.luxury
				p:*Sqr(bp.output.luxury)
			EndIf
			debugo "probability of building "+bp.output.name+" factory = "+String(1-p)
			If Rnd(0,1)>p
				debugo "building!"
				m:merchant=merchant.Create(pickname(),home)
				tycoons.addlast m.name
				money:-tcost
				m.money:+tcost
				m.build(bp)
			EndIf
		Next
		
	End Method
End Type

Type letter
	Field txt$
	Field sentences:TList
	Field destination$
	Field recipient$
	Field sender$
	Field shippingtime
	Field numinfo
	Field fonts:tmap
	
	Method New()
		sentences=New TList
	End Method
	
	Method jsonise:jsonvalue()
		j:jsonobject=New jsonobject
		j.addnewpair("txt",jsonstringvalue.Create(txt))
		j.addnewpair("destination",jsonstringvalue.Create(destination))
		j.addnewpair("recipient",jsonstringvalue.Create(recipient))
		j.addnewpair("sender",jsonstringvalue.Create(sender))
		j.addnewpair("shippingtime",jsonnumbervalue.Create(shippingtime))
		j.addnewpair("numinfo",jsonnumbervalue.Create(numinfo))
		sa:jsonarray=New jsonarray
		j.addnewpair("sentences",sa)
		For s:sentence=EachIn sentences
			sa.values.addlast s.jsonise()
		Next
		Return j
	End Method
	
	Function Load:letter(j:jsonobject)
		recipient$=j.getstringvalue("recipient")
		sender$=j.getstringvalue("sender")
		destination$=j.getstringvalue("destination")
		let:letter=letter.Create(recipient,sender,destination,Null)
		let.txt=j.getstringvalue("txt")
		let.shippingtime=j.getnumbervalue("shippingtime")
		let.numinfo=j.getnumbervalue("numinfo")
		sa:jsonarray=j.getarrayvalue("sentences")
		For so:jsonobject=EachIn sa.values
			let.sentences.addlast sentence.Load(so)
		Next
		Return let
	End Function
	
	Function Create:letter(recipient$,sender$,destination$,fonts:tmap)
		l:letter=New letter
		l.recipient=recipient
		l.destination=destination
		l.sender=sender
		l.fonts=fonts
		
		Return l
	End Function
	
	Method addsentence(s:sentence,info=1)
		s.txt=prettysentence(s.txt)
		txt:+s.txt
		If Rand(4)=1
			txt:+"~n~n~t"
		Else
			txt:+" "
		EndIf
		numinfo:+info
		sentences.addlast s
	End Method
	
	Method addemptysentence(in$,info=1)
		s:sentence=New sentence
		s.txt=in
		addsentence s,info
	End Method
	
End Type

Type stock
	Field amount#
	
	Method jsonise:jsonvalue()
		Return jsonnumbervalue.Create(amount)
	End Method
	
	Function Load:stock(v:jsonvalue)
		s:stock=New stock
		s.amount=jsonnumbervalue(v).number
		Return s
	End Function
End Type

Type port
	Field name$
	Field taxrate#
	'Field prices:tmap
	'Field warehouse:tmap
	Field inbox:TList
	Field outbox:TList
	Field distances:tmap
	Field distance#,prvport:port
	Field residents:TList
	Field journal:TList,reported
	Field populace:pleb
	Field taxdate
	Field economy#
	
	Method New()
		prices=New tmap
		inbox=New TList
		outbox=New TList
		distances=New tmap
		s:stock=New stock
		s.amount=0
		distances.insert(Self,s)
		residents=New TList
		journal=New TList
	End Method
	
	Method jsonise:jsonvalue()
		j:jsonobject=New jsonobject
		j.addnewpair("name",jsonstringvalue.Create(name))
		j.addnewpair("taxrate",jsonnumbervalue.Create(taxrate))
		ij:jsonarray=New jsonarray
		j.addnewpair("inbox",ij)
		For let:letter=EachIn inbox
			ij.values.addlast let.jsonise()
		Next
		oj:jsonarray=New jsonarray
		j.addnewpair("outbox",oj)
		For let:letter=EachIn outbox
			oj.values.addlast let.jsonise()
		Next
		dj:jsonobject=New jsonobject
		j.addnewpair("distances",dj)
		For hname$=EachIn distances.keys()
			dj.addnewpair(hname,stock(distances.valueforkey(hname)).jsonise())
		Next
		rj:jsonarray=New jsonarray
		j.addnewpair("residents",rj)
		For r:resident=EachIn residents
			If r<>populace
				rj.values.addlast r.jsonise()
			EndIf
		Next
		nj:jsonarray=New jsonarray
		j.addnewpair("journal",nj)
		For news$=EachIn journal
			nj.values.addlast jsonstringvalue.Create(news)
		Next
		j.addnewpair("reported",jsonnumbervalue.Create(reported))
		j.addnewpair("populace",populace.jsonise())
		j.addnewpair("taxdate",jsonnumbervalue.Create(taxdate))
		j.addnewpair("economy",jsonnumbervalue.Create(economy))
		Return j
	End Method
	
	Function Load:port(j:jsonobject)
		name$=j.getstringvalue("name")
		taxrate#=j.getnumbervalue("taxrate")
		h:port=port.Create(name,taxrate)
		ij:jsonarray=j.getarrayvalue("inbox")
		For io:jsonobject=EachIn ij.values
			h.inbox.addlast letter.Load(io)
		Next
		oj:jsonarray=j.getarrayvalue("outbox")
		For oo:jsonobject=EachIn oj.values
			h.outbox.addlast letter.Load(oo)
		Next
		dj:jsonobject=j.getobjectvalue("distances")
		For p:jsonpair=EachIn dj.pairs
			h.distances.insert p.name,stock.Load(p.value)
		Next
		rj:jsonarray=j.getarrayvalue("residents")
		For ro:jsonobject=EachIn rj.values
			Select ro.getstringvalue("type")
			Case "merchant"
				merchant.Load(ro)
			Case "farmer"
				farmer.Load(ro)
			Case "factory"
				factory.Load(ro)
			End Select
		Next
		nj:jsonarray=j.getarrayvalue("journal")
		For ns:jsonstringvalue=EachIn nj.values
			h.journal.addlast ns.txt
		Next
		h.reported=j.getnumbervalue("reported")	
		h.populace=pleb.Load(j.getobjectvalue("populace"))
		h.taxdate=j.getnumbervalue("taxdate")
		h.economy=j.getnumbervalue("economy")
		Return h
	End Function
	
	Function link(h1:port,h2:port,distance#)
		s:stock=New stock
		s.amount=distance
		h1.distances.insert(h2.name,s)
		h2.distances.insert(h1.name,s)
	End Function
	
	Function Create:port(name$,taxrate#, population=-1)
		h:port=New port
		h.name=name
		h.taxrate=taxrate
		ports.insert(h.name,h)
		If population>=0
			h.populace=pleb.Create(h,population)
			h.populace.movestock(product.find("beef"),population*10/50.0)
			h.populace.money=population*30/100.0
			h.populace.name="the citizens of "+h.name
		EndIf
		Return h
	End Function
	
	Function find:port(name$)
		If ports.contains(name)
			Return port(ports.valueforkey(name))
		EndIf
	End Function
	
	Method findresident:resident(rname$)
		For r:resident=EachIn residents
			If r.name=rname
				Return r
			EndIf
		Next
	End Method
	
	Rem
	Method getprice:stock(p:product)
		If Not prices.contains(p)
			s:stock=New stock
			s.amount=1
			prices.insert(p,s)
		EndIf
		Return stock(prices.valueforkey(p))
	End Method
	EndRem
	
	Method priceof#(p:product)
		total#=0
		number=0
		For r:resident=EachIn residents
			rprice#=r.priceof(p)
			rstock=r.numforsale(p)
			If rprice>=0
				total:+rstock*rprice
				number:+rstock
			EndIf
		Next
		If number>0
			price#=total/number
		Else
			price#=0
		EndIf
		Return price
	End Method
	
	Method addnews(txt$) 
		If Not reported
			journal.addlast datename() 
			reported = 1
		EndIf
		journal.addlast "~t"+prettysentence(txt)
	End Method
	
	Method getnews$()
		inserts:tmap=New tmap
		inserts.insert "name",name
		out$=doinserts(gettemplate("news"),inserts)
		For news$=EachIn journal
			out:+news+"~n~n"
		Next
		out:+"~n"
		out:+String(populace.population)+" "+pluralise("person",populace.population,"people")+" reside in "+name+".~n"
		out:+ "The " + name + " economy is worth " + moneystring(economy) + ".~n~n"
		forsale:TList=New TList
		inserts=New tmap
		col1$=""
		col2$=""
		For p:product = EachIn product.all()
			If stockof(p) 
				forsale.addlast p.plural
				
				If col1
					col1:+"~n~n"
					col2:+"~n~n"
				EndIf
				col1:+p.plural
				col2:+moneystring(priceof(p))
			EndIf
		Next
		inserts.insert "col1",col1
		inserts.insert "col2",col2
		If forsale.count()
			out:+doinserts(gettemplate("stocknews"),inserts)
			'out:+ "The markets here sell " + prettylist(forsale) + ".~n~n"
		Else
			out:+"Nothing is being sold here.~n~n"
		EndIf
		out:+"^ ~qrule~q: 100 ^"
		Print out
		journal=New TList
		Return out
	End Method
		
	Rem
	Method setprice(p:product,amount#)
		getprice(p).amount=amount
		'report "The price of "+p.plural+" in "+name+" is now "+moneystring(priceof(p))
	End Method
	EndRem
	
	Rem
	Method getstock:stock(p:product)	
		If Not warehouse.contains(p)
			warehouse.insert(p,New stock)
		EndIf
		Return stock(warehouse.valueforkey(p))
	End Method
	EndRem
	
	Method stockof(p:product) 
		total = 0
		For r:resident = EachIn residents
			total:+ r.stockof(p) 
		Next
		Return total
	End Method
	
	Method numforsale(p:product)
		total = 0
		For r:resident = EachIn residents
			total:+ r.numforsale(p) 
		Next
		Return total
	End Method
	
	Rem
	Method movestock(p:product,number)
		s:stock=getstock(p)
		s.amount:+number
	End Method
	EndRem

	Method sendletter(let:letter)
		If let.destination=name
			inbox.addlast let
			time=1
		Else
			outbox.addlast let
			dest:port=port.find(let.destination)
			findroute(dest)
			nextport:port=nextpost(dest)
			time=dest.distance
			'report let.sender+" sent a letter to "+let.recipient+" taking "+String(time)+" "+pluralise("day",time)
		EndIf
		Return time
	End Method

	Method findroute(h:port)
		q:TList=New TList
		For h2:port=EachIn ports.values()
			q.addlast h2
			h2.distance=-1
			h2.prvport=Null
		Next
		distance=0
		
		While q.count()
			mindist=-1
			curport:port=Null
			For h2:port=EachIn q
				If (h2.distance<mindist Or mindist=-1) And h2.distance>=0
					mindist=h2.distance
					curport=h2
				EndIf
			Next
			q.remove curport
			For hname$=EachIn curport.distances.keys()
				h2:port=port.find(hname)
				alt=curport.distance+stock(curport.distances.valueforkey(hname)).amount
				If alt<h2.distance Or h2.distance=-1
					h2.distance=alt
					h2.prvport=curport
				EndIf
			Next
		Wend
	End Method
		
	Method nextpost:port(h:port)
		If h=Self Then Return h
		findroute(h)
		
		curport:port=h
		While curport.prvport<>Self
			curport=curport.prvport
		Wend
		Return curport
	End Method
	
	Method update()
		'If name="London" debugging=1 Else debugging=0
		debugging=1
		
		debugo "--"+name.toupper()+"   pop. "+String(populace.population)
		economy# = 0
		For r:resident=EachIn residents
			economy:+r.money
		Next
		debugo "economy: "+moneystring(economy)
		
		If date>=taxdate
			collecttax()
			taxdate=date+30
		EndIf
		
		deliverin
		
		reported=0
		
		For r:resident=EachIn residents
			If r<>player
				r.update
			EndIf
		Next
	End Method	
	
	Method deliverin()
		For let:letter=EachIn inbox
			let.shippingtime:-1
			If let.shippingtime<=0
				dest:port=port.find(let.destination)
				If dest=Self
					'report "delivered a letter to "+let.recipient.name
					merchant.find(let.recipient).deliverletter let
				Else
					'report "forwarding letter bound for "+dest.name
					outbox.addlast let
				EndIf
				inbox.remove let
			EndIf
		Next
	End Method
	
	Method collecttax()
		'Return
		debugo "collecting tax"
		total#=0
		For r:resident=EachIn residents
			If r.money>0
				tax#=taxrate*r.money
				total:+tax
				r.money:-tax
				populace.money:+tax
			EndIf
		Next
		debugo "collected "+moneystring(total)+" tax"
	End Method

	Method deliverout()
		destinations:tmap=New tmap
		ships:tmap=New tmap
		For let:letter=EachIn outbox
			If Not destinations.contains(let.destination)
				destinations.insert(let.destination,New TList)
			EndIf
			TList(destinations.valueforkey(let.destination)).addlast let
		Next
		For hname$=EachIn destinations.keys()
			h:port=port.find(hname)
			nextport:port=nextpost(h)
			l:TList=TList(destinations.valueforkey(hname))
			If Not ships.contains(nextport)
				ships.insert(nextport, New TList)
			EndIf
			For let:letter=EachIn l
				TList(ships.valueforkey(nextport)).addlast let
			Next
		Next
		For h:port=EachIn ships.keys()
			l:TList=TList(ships.valueforkey(h))
			distance=stock(distances.valueforkey(h.name)).amount
			For let:letter=EachIn l
				h.inbox.addlast let
				let.shippingtime=distance
			Next
			numletters=l.count()
			'report "shipped "+String(numletters)+" "+pluralise("letter",numletters)+" to "+nextport.name
		Next
		outbox=New TList
	End Method
End Type

Function clue$(w$)
	Select w
	Case "product"
		Return "the name of a product"
	Case "pluralnumber"
		Return "a number"
	Case "number"
		Return "a number"
	Case "percentage"
		Return "a percentage"
	Case "money","strictmoney"
		Return "an amount of money"
	Case "name"
		Return "a name"
	Case "merchant","mymerchant"
		Return "the name of a merchant"
	Case "employee"
		Return "one of your agents"
	Case "port"
		Return "the name of a port"
	Case "output"
		Return "a product which can be made in a factory"
	Case "day"
		Return "a day number"
	Case "month"
		Return "a month of the year"
	End Select
End Function

Function guesses:TList(kind$)
	l:TList=New TList
	Select kind$
	Case "muchproduct"
		For p:product=EachIn product.all()
			If p.name=p.plural
				l.addlast p.name
			EndIf
		Next
	Case "manyproduct"
		For p:product=EachIn product.all()
			If p.name<>p.plural
				l.addlast p.plural
			EndIf
		Next
	Case "singleproduct"
		For p:product=EachIn product.all()
			l.addlast p.name
		Next
	Case "pluralproduct"
		For p:product=EachIn product.all()
			l.addlast p.plural
		Next
	Case "product"
		For name$=EachIn products.keys()
			l.addlast name
		Next
	Case "output"
		For p:product=EachIn blueprints.keys()
			l.addlast p.plural
		Next
	Case "merchant","othermerchant"
		For name$=EachIn merchants.keys()
			If name<>"Player"
				l.addlast name
			ElseIf kind="merchant"
				l.addlast "me"
			EndIf
		Next
	Case "mymerchant"
		For name$=EachIn player.contacts
			l.addlast name
		Next
	Case "employee"
		For name$=EachIn player.employees
			l.addlast name
		Next
	Case "port"
		For name$=EachIn ports.keys()
			l.addlast name
		Next
	Case "month"
		For i=0 To 11
			l.addlast monthname[i]
		Next
	Default
		l.addlast "??"+kind
	End Select
	l.sort
	Return l
End Function	

Function validinput$(kind$,in$,strictmatch=1)
	Select kind
	Case "product","singleproduct","pluralproduct"
		If products.contains(in)
			Return in
		Else
			Return ""
		EndIf
	Case "output"
		p:product=product.find(in)
		If Not p Return ""
		If blueprints.contains(product.find(in))
			Return in
		Else
			Return ""
		EndIf
	Case "merchant","othermerchant"
		If in="me"
			Return in
		EndIf
		If merchants.contains(in)
			Return in
		Else
			Return ""
		EndIf
	Case "mymerchant"
		If player.contacts.contains(in)
			Return in
		Else
			Return ""
		EndIf
	Case "employee"
		If player.employees.contains(in)
			Return in
		Else
			Return ""
		EndIf
	Case "port"
		If ports.contains(in)
			Return in
		Else
			Return ""
		EndIf
	Case "number"
		For i=0 To Len(in)-1
			c=in[i]
			If c<>46 And (c<48 Or c>57)
				Return ""
			EndIf
		Next
		Return in
	Case "pluralnumber"
		If validinput("number",in)
			If Float(in)>1
				Return in
			EndIf
		EndIf
		Return ""
	Case "money","strictmoney"
		If in="" Then Return ""
		If in[0]=36
			If Len(in)=1 Then Return in
			numvalid$=validinput("number",in[1..])
			If numvalid
				Return moneystring(Float(numvalid))
			Else
				Return ""
			EndIf
		ElseIf kind="strictmoney"
			Return ""
		EndIf
		numvalid$=validinput("number",in)
		If numvalid
			Return moneystring(Float(numvalid))
		Else
			Return ""
		EndIf
		
	Case "percentage"
		If in="" Then Return in
		If in[Len(in)-1]<>37
			If Not strictmatch
				Return validinput("number",in)
			Else
				Return ""
			EndIf
		EndIf
		Return validinput("number" , in[..Len(in) - 1]) + "%"
	Case "day"
		c = 0
		While c < Len(in) And in[c] >= 48 And in[c] <= 57 
			c:+ 1
		Wend
		num = Int(in[..c]) 
		If ordinal(num)[..Len(in)]=in
			Return ordinal(num)
		Else
			Return ""
		EndIf
	Case "month"
		For i = 0 To 11
			If monthname[i] = in Return in
		Next
		Return ""
	Case "name"
		Return in
	Default
		Return in
	End Select
End Function

Function pickname$()
	name$=names[Rand(Len(names))-1]
	While merchant.find(name)
		name$=names[Rand(Len(names))-1]
	Wend
	Return name
End Function

Function report(txt$,fonts:tmap=Null,width#=-1)
	If width>0
		txt="^ ~qcolumn~q: {~qnumber~q: 1, ~qwidth~q: "+String(Int(width*100/scrollerwidth))+"}^"+txt+"~n~n^ ~qendcolumn~q:1^"
	EndIf
	txt=doinserts(txt,New tmap)
	'If Not fonts
	'	fonts=New tmap
	'	'fonts.insert "handwriting",handwritingfonts[0]
	'	fonts.insert "print",printfonts[0]
	'	fonts.insert "headline",headlinefonts[0]
	'	txt="^ ~qsize~q : 80 ^"+txt
	'EndIf
	tb:textblock=textblock.Create(txt,scrollerwidth,.5,fonts)
	tb.render scrolllength
	For tl:typeline=EachIn tb.typelines
		lines.addlast tl
	Next
	scrolllength=tb.y+10
End Function

Type background
	Field starty,endy
	Field startx,endx
	
	Method draw(xoff,yoff)
	End Method
End Type

Type bgrect Extends background
	Field r,g,b
	
	Function Create:bgrect(startx,endx,starty,endy,r,g,b)
		br:bgrect=New bgrect
		br.startx=startx
		br.endx=endx
		br.starty=starty
		br.endy=endy
		br.r=r
		br.g=g
		br.b=b
		Return br
	End Function
	
	Method draw(xoff,yoff)
		SetScale 1,1
		SetColor r,g,b
		DrawRect startx+xoff,starty+yoff,endx-startx,endy-starty
	End Method
End Type

Function doinserts$(txt$,inserts:tmap)
	i=0
	out$=""
	While i<Len(txt)
		If txt[i]=35
			out:+txt[..i]
			i:+1
			txt=txt[i..]
			i=0
			While txt[i]<>35
				i:+1
			Wend
			iname$=txt[..i].Trim()
			If inserts.contains(iname)
				insert$=String(inserts.valueforkey(iname))
			Else
				insert$=getinsert(iname)
			EndIf
			out:+insert
			txt=txt[i+1..]
			i=0
		EndIf
		i:+1
	Wend
	Return out+txt
End Function

Function getinsert$(iname$)
	Select iname
	Case "datename"
		Return datename()
	End Select
End Function


Function letterwrite(s:sentence)
	Print s.repr()
	br:bgrect=bgrect(bgs.last())
	command$=s.nextparam(0)
	Select command
	Case "yours"
		s.nextparam
		
		valediction$=s.nextparam()
		letterlen=Len(curletter.txt)
		player.finishletter(curletter,valediction)
		report gettemplate("playerletter")+Trim(curletter.txt[letterlen..]),player.fonts,scrollerwidth-50
		br.endy=scrolllength+20
		scrolllength:+30
		dtime=player.home.sendletter(curletter)
		report "The letter will take "+String(dtime)+pluralise(" day",dtime)+" to get to "+curletter.recipient

		changegrammar "main"
	Case "cancel"
		cancelletter
	Default
		curletter.addsentence(s)
		report gettemplate("playerletter")+s.txt,player.fonts,scrollerwidth-50
		scrolllength:+30
		br.endy=scrolllength+20
	End Select
End Function

Function cancelletter()
	curletter=Null
	changegrammar "main"
	scrolllength:+30
	report "Letter scrapped"
End Function

Function maininterpret(s:sentence)
	command$=s.nextparam()
	Select command
	Case "write"
		name$=s.nextparam()
		If Not name
		Else			
			scrolllength:+30
			m:merchant=merchant.find(name)
			report "Writing a letter to "+m.name+"~n"
			br:bgrect=bgrect.Create(10,scrollerwidth-10,scrolllength,scrolllength,248,236,194)
			bgs.addlast br
			scrolllength:+20
			curletter=player.startletter(m)
			report Trim(curletter.txt),player.fonts,scrollerwidth-50
			scrolllength:+30
			If player.employees.contains(m.name)
				changegrammar "letter"
			Else
				changegrammar "contractletter"
			EndIf
			br.endy=scrolllength+20
		EndIf
	Case "sleep"
		sleep
		checkmail
	Case "read"
		For let:letter=EachIn player.inbox
			showletter let
		Next
		player.inbox=New TList
	Case "hire"
		pname$=s.nextparam()
		home:port=port.find(pname)
		mname$=pickname()
		m:merchant=merchant.Create(mname,home)
		m.money:+player.money
		player.money=0
		let:letter=player.startletter(m)
		ns:sentence=New sentence
		ns.addparam("$","hired")
		ns.txt="You have been hired."
		let.addsentence ns
		player.finishletter(let,"sincerely")
		player.home.sendletter let
		report "You have hired " + mname + " as your agent in " + pname+". He will write to you when he is ready to trade"
	Case "wait"
		Print s.repr()
		unt$=s.nextparam()
		Print "WAIT "+unt
		Select unt
		Case "dayis" 'wait until given date
			dname$ = s.getparam("day") 
			mname$ = s.getparam("month") 
			wmonth = resolvemonth(mname) 
			wday = resolveday(dname)
			wdate = backdate(year , wmonth , wday)
			If wdate < date Then wdate = backdate(year + 1 , wmonth , wday) 
			diff = wdate - date
			startletters=player.inbox.count()
			For i = 1 To diff - 1
				sleep
			Next
			sleep
		Case "letterreceived" 'wait until letter received
			startletters=player.inbox.count()
			n=0
			While player.inbox.count()=startletters And n<365
				sleep
				n:+1
			Wend
		Case "dayselapsed"
			numdays=Int(s.nextparam())
			For i=1 To numdays
				sleep
			Next
		End Select
		checkmail
	Case "give"
		mname$=s.nextparam()
		money#=resolveamount(s.nextparam())
		merchant.find(mname).money:+money
	Case "menu"
		changegrammar "menu"
	Case "map"
		changemode New mapmode
	End Select
End Function

Function showletter(let:letter)
	oy=scrolllength
	scrolllength:+30
	report let.txt,let.fonts,scrollerwidth-50
	scrolllength:+20
	bgs.addlast bgrect.Create(10,gwidth-10,oy,scrolllength,248,236,194)
	scrolllength:+10
End Function

Function menuinterpret(s:sentence)
	command$=s.nextparam()
	Select command
	Case "new"
		initgame
	Case "save"
		savegame
		report "Game saved to save.txt"
		changegrammar "main"
	Case "load"
		initgame
		loadgame ReadFile("save.txt")
		report "It is "+datename()
		changegrammar "main"
	Case "back"
		changegrammar "main"
	Case "quit"
		savegame
		curmode=Null
	End Select
End Function

Function changegrammar(gname$)
	curgrammar=grammar(grammars.valueforkey(gname))
	curgrammar.init
End Function

Function sleep()
	debugging=1
	debugo "~n***********~n"
	
	For h:port=EachIn ports.values()
		h.update
	Next
	For h:port=EachIn ports.values()
		h.deliverout
	Next
	
	date:+1
	calcdate

End Function

Function checkmail()
	report "It is "+datename()
	lcount=player.inbox.count()
	If lcount
		report "^ ~qalign~q: ~qcentre~q ^You have "+String(player.inbox.count())+" unread "+pluralise("letter",lcount)
	EndIf
End Function


Incbin "consolai.ttf"

Function savegame(f:TStream=Null)
	j:jsonobject=New jsonobject
	pj:jsonarray=New jsonarray
	j.addnewpair("products",pj)
	pl:TList=New TList
	For p:product=EachIn products.values()
		If Not pl.contains(p) pl.addlast p
	Next
	For p:product=EachIn pl
		pj.values.addlast p.jsonise()
	Next
	bj:jsonarray=New jsonarray
	j.addnewpair("blueprints",bj)
	For bp:blueprint=EachIn blueprints.values()
		bj.values.addlast bp.jsonise()
	Next
	hj:jsonarray=New jsonarray
	j.addnewpair("ports",hj)
	For h:port=EachIn ports.values()
		hj.values.addlast h.jsonise()
	Next
	j.addnewpair("date",jsonnumbervalue.Create(date))
	
	j.addnewpair("player",jsonstringvalue.Create(player.name))
	
	mj:jsonarray=New jsonarray
	j.addnewpair("tycoons",mj)
	For mname$=EachIn tycoons
		mj.values.addlast jsonstringvalue.Create(mname)
	Next
	'mj:jsonarray=New jsonarray
	'j.addnewpair("merchants",mj)
	'For m:merchant=EachIn merchants.values()
	'	mj.values.addlast jsonstringvalue.Create(m.name)
	'Next
	
	If Not f
		f=WriteFile("save.txt")
	EndIf
	
	'debugo j.repr()
	f.WriteString j.repr()
	For line$=EachIn j.repr().split("~n")
	'	debugo line
		'f.WriteLine line
	Next
	
	CloseFile f	
End Function

Function loadgame(f:TStream=Null)
	If Not f
		f=WriteFile("save.txt")
	EndIf
	
	txt$=""
	While Not Eof(f)
		txt:+f.ReadLine()
	Wend
	
	jd:jsondecoder=jsondecoder.Create(txt)
	jd.parse()
	
	j:jsonobject=jsonobject(jd.things.first())
	
	pj:jsonarray=j.getarrayvalue("products")
	For po:jsonobject=EachIn pj.values
		product.Load(po)
	Next
	
	bj:jsonarray=j.getarrayvalue("blueprints")
	For bo:jsonobject=EachIn bj.values
		blueprint.Load(bo)
	Next
	
	hj:jsonarray=j.getarrayvalue("ports")
	For po:jsonobject=EachIn hj.values
		port.Load(po)
	Next
	
	date=j.getnumbervalue("date")
	player=merchant.find(j.getstringvalue("player"))
	
	mj:jsonarray=j.getarrayvalue("tycoons")
	For ms:jsonstringvalue=EachIn mj.values
		tycoons.addlast ms.txt
	Next
	
	
	CloseFile f
End Function

Function initgame() 
	
	clickables=New TList

	products=New tmap
	ports=New tmap
	blueprints=New tmap
	merchants=New tmap
	tycoons=New TList
	
	
	
	loadnames(ReadFile("names.txt"))

	loadgrammars

	changegrammar "main"

	lines=New TList
	bgs=New TList
	output=""
	
	curletter=Null
	
	lastscroll=MilliSecs()

End Function

Function loadworld(f:TStream)
	date=0
	calcdate

	product.Create("cow","cattle",0)
	product.Create("beef","beef",0)

	Local words$[]
	While Not Eof(f)
		line$=ReadLine(f)
		Select line
		Case "products"
			line=ReadLine(f)
			While line<>""
				words=line.split(" ")
				name$=words[0]
				plural$=words[1]
				If Len(words)>2
					luxury#=Float(words[2])
				Else
					luxury#=0
				EndIf
				product.Create(name,plural,luxury)
				line=ReadLine(f)
			Wend
		Case "ports"
			line=ReadLine(f)
			While line<>""
				words=line.split(" ")
				name$=words[0]
				taxrate#=Float(words[1])
				population=Int(words[2])
				port.Create(name,taxrate,population)
				line=ReadLine(f)
			Wend
		Case "distances"
			line=ReadLine(f)
			While line<>""
				words=line.split(" ")
				h1:port=port.find(words[0])
				h2:port=port.find(words[1])
				port.link(h1,h2,Int(words[2]))
				line=ReadLine(f)
			Wend
		Case "merchants"
			line=ReadLine(f)
			While line<>""
				words=line.split(" ")
				h:port=port.find(words[1])
				m:merchant=merchant.Create(words[0],h)
				line=ReadLine(f)
			Wend
		Case "farmers"
			line=ReadLine(f)
			While line<>""
				words=line.split(" ")
				h:port=port.find(words[0])
				p:product=product.find(words[1])
				harvestmonth=resolvemonth(words[2])
				harvest=Int(words[3])
				startprice#=Float(words[4])
				farmer.Create(h,p,startprice,harvestmonth,harvest)
				line=ReadLine(f)
			Wend
		Case "factories"
			line=ReadLine(f)
			While line<>""
				words=line.split(" ")
				p:product=product.find(words[0])
				cost#=Float(words[1])
				rates:tmap=New tmap
				n=2
				While n<Len(words)
					rate#=Float(words[n])
					ip:product=product.find(words[n+1])
					s:stock=New stock
					s.amount=rate
					rates.insert(ip,s)
					n:+2
				Wend
				blueprint.Create(p,cost,rates)
				line=ReadLine(f)
			Wend
		End Select
	Wend
	
	player=merchant.Create("Player",port.find("London"))
	player.money=500

	CloseFile f

	savegame WriteFile("world2.txt")
End Function

Function loadnames(f:TStream)
	lines:TList=New TList
	While Not Eof(f)
		line$=f.ReadLine().Trim()
		If line Then lines.addlast line
	Wend
	names=New String[lines.count()]
	For i=0 To lines.count()-1
		names[i]=String(lines.removefirst())
	Next
End Function

Function loadgrammars()
	grammars=New tmap
	dirhandle=ReadDir("grammars/")
	fname$=NextFile(dirhandle)
	While fname
		If fname[0]<>46 And Len(fname)>4
			grammars.insert fname[..Len(fname)-4],grammar.fromfile(ReadFile("grammars/"+fname),fname[..Len(fname)-4])
		EndIf
		fname=NextFile(dirhandle)
	Wend
	'maingrammar=grammar.fromfile(ReadFile("maingrammar.txt"))
	grammar.find("main").interpret=maininterpret
	'lettergrammar=grammar.fromfile(ReadFile("lettergrammar.txt"))
	grammar.find("letter").interpret=letterwrite
	'menugrammar=grammar.fromfile(ReadFile("menugrammar.txt"))
	grammar.find("menu").interpret=menuinterpret
	'contractlettergrammar=grammar.fromfile(ReadFile("contractlettergrammar.txt"))
	grammar.find("contractletter").interpret=letterwrite
End Function

Type tradermode Extends gamemode
	Field lastscroll
	Method New()
		scrolling=1
		scroll# = 0
		scrolllength=0
		startgfx
		lastscroll=MilliSecs()
	End Method
	
	Method startgfx()
		gwidth=600
		gheight = 800

		scrollerheight=600
		scrollerwidth=gwidth

		AppTitle="Amsterdammit"
		Graphics gwidth,gheight,0
		SetBlend alphablend
		SetClsColor 255 , 255 , 255
	End Method	
	
	Method arrive()
		startgfx
		SeedRnd MilliSecs()
	End Method
		
	Method update()
		ms=MilliSecs()
		
		curgrammar.update()
	
		n=0
		For bg:background=EachIn bgs
			n:+1
			If bg.endy-scroll>0 And bg.starty-scroll<scrollerheight
				bg.draw(0,-scroll)
			EndIf
		Next
	
		SetColor 0,0,0
		For tl:typeline=EachIn lines
			If tl.y+tl.height-scroll>0 And tl.y-scroll<scrollerheight
				tl.draw(0,-scroll)
			EndIf
		Next
	
		If scroll>0	
			If KeyDown(KEY_UP) 
				scroll:- 8
				If scroll<0 Then scroll=0
				scrolling = 0
				lastscroll=ms
			EndIf
		EndIf
		If scrolllength-scroll>scrollerheight
			If scrolling
				scroll:+ 2
			EndIf
			If KeyDown(KEY_DOWN) 
				scroll:+ 8
				scrolling = 0
				lastscroll=ms
			EndIf
		Else
			scrolling=1
		EndIf
		If ms > lastscroll + 3000
			scrolling = 1
		EndIf
		
		
		SetScale 1,1
		SetColor 255,255,255
		DrawRect 0,scrollerheight,gwidth,gheight-scrollerheight
		SetColor 255 , 0 , 0
		SetLineWidth 3
		DrawLine 0,scrollerheight,gwidth,scrollerheight
		
		curgrammar.draw(0,scrollerheight+10)
		
		Flip
		Cls
	
	End Method
End Type
