val a =Seq(2,2,2,2)
a.scan(1)(_*_).indexWhere(_ == 8)
a.splitAt(3)
