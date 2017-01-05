package io.github.vigoo.barlang.language

import io.github.vigoo.barlang.prettyprinter.PrettyPrinter
import io.github.vigoo.barlang.language.PrettyPrinterInstances._

object Play extends App {
  val src = """|return "alma"
               |return "korte"
               |return true
               |return false
               |return 11
               |return -0.5
               |return x
               |return $HOME
               |return something
               |return sinTable[10]
               |return cosTable[i]
               |return f(1)
               |return f(g(h(0)))
               |return (get("something"))
               |return 1+2+3
               |return 10+2*3
               |return (10+2)*3
               |return 6*6
               |return 0/0
               |return not false
               |return true or false
               |return true or false and not true
               |return f(1) + g(2 mod 3)
               |return print(true, false, 100 * 100, "and this")
               |return fn (x: int): int
               |  return x * 2
               |end
               |return fn [T1, T2] (x: T1, y: T2): int
               |  return -1
               |end
               |return fn (table: [double], idx: int): double
               |  return table[idx]
               |end
               |return fn [T] (table: T, getter: (T, int) -> double, idx: int): double
               |  return getter(table, idx)
               |end
               |return fn [T] (table: T, getter: [T] (T, int) -> double, idx: int): double
               |  return getter(table, idx)
               |end
               |val width = 100
               |val hello = "World"
               |val f = fn (who: string): unit
               |  val x = 10
               |  return print(hello)
               |end
               |inline def print[T](s: T): unit
               |  > "echo" str(s)
               |end
               |inline def cls(): unit
               |  > "tput" "clear"
               |end
               |def putpixel(x: int, y: int, c: int, d: int): unit
               |  gotoxy(x, y)
               |  color(c, 0)
               |  if d > 0 then
               |    print(chars[d - 1])
               |  else
               |    print(" ")
               |  end
               |end
               |while x <= 10
               |  val y = x * 2
               |  x <- x+1
               |end
               |
               |array[double] sinTable
               |while i < res
               |  sinTable[i] <- scale * sin(((2*pi)*i)/res)
               |  i <- i + 1;
               |end
               |""".stripMargin

  val tokens = Parser.BarlangLexer.parse(Parser.BarlangLexer.tokens, src)
//  println(s"Tokens:")
//  tokens.get.foreach(t => println(s"${t.pos.line}:${t.pos.column} $t"))

  val result = Parser.BarlangParser(tokens.get)
  result match {
    case Parser.BarlangParser.Success(ast, next) =>
      println(ast.body.mkString("\n"))
      println("===")
      println(PrettyPrinter(ast))
    case Parser.BarlangParser.NoSuccess(msg, next) =>
      println(s"${next.pos.line}:${next.pos.column} $msg")
      println(src.lines.toVector(next.pos.line-1))
  }


  val gradientSrc =
    """
      |inline def cls(): unit
      |    > "tput" "clear";
      |end;
      |
      |inline def gotoxy(cx: int, cy: int): unit
      |    > "echo" "-en" ("\\033[" + str(cy) + ";" + str(cx) + "H");
      |end;
      |
      |inline def print[T](s: T): unit
      |	> "echo" (str(s));
      |end;
      |
      |inline def color(fg: int, bg: int): unit
      |    > "tput" "setf" (str(fg));
      |	> "tput" "setb" (str(bg));
      |end;
      |
      |inline def waitkey(): unit
      |    > "read";
      |end;
      |
      |inline def hideCursor(): unit
      |    > "tput" "civis";
      |end;
      |
      |array[string] chars;
      |chars[0] <- ".";
      |chars[1] <- ":";
      |chars[2] <- "-";
      |chars[3] <- "=";
      |chars[4] <- "+";
      |chars[5] <- "m";
      |chars[6] <- "#";
      |chars[7] <- "%";
      |chars[8] <- "W";
      |
      |array[int] colors;
      |colors[0] <- 4;
      |colors[1] <- 6;
      |colors[2] <- 7;
      |
      |def putpixel(x: int, y: int, c: int, d: int): unit
      |	gotoxy(x, y);
      |	color(c, 0);
      |	if (d > 0) then
      |		print(chars[d - 1]);
      |	else
      |		print(" ");
      |	end;
      |end;
      |
      |
      |val width = 80;
      |val height = 40;
      |
      |def clamp(v: int, max: int): int
      |	 val res = v;
      |     if (v > max) then
      |	     res <- max;
      |     else
      |	     res <- v;
      |     end;
      |	 return res;
      |end;
      |
      |def scene1(N: int, sinTable: [double]): unit
      |    cls();
      |    val y = 0;
      |    val s = N / width;
      |    while (y < height)
      |	  val x = 0;
      |	  while (x < width)
      |			val c = toInt(((sinTable[x * s] / 20.0) + 1) * x + y) mod 30;
      |			val col = colors[c / 10];
      |			val dens = c / 3;
      |			putpixel(x, y, col, dens);
      |	  		x <- x + 1;
      |	  end;
      |	  y <- y + 1;
      |    end;
      |end;
      |
      |def scene2(N: int, sinTable: [double]): unit
      |    cls();
      |    val y = 0;
      |    val s = N / height;
      |    while (y < height)
      |	  val x = 0;
      |	  while (x < width)
      |	  		val r = sinTable[(y * s * 5) mod 1000] / 2.0;
      |			val c = toInt(x + y + r) mod 30;
      |			val col = colors[c / 10];
      |			val dens = c / 3;
      |			putpixel(x, y, col, dens);
      |	  		x <- x + 1;
      |	  end;
      |	  y <- y + 1;
      |    end;
      |end;
      |
      |def scene3(N: int, sinTable: [double]): unit
      |	cls();
      |    val y = 0;
      |    while (y < height)
      |	  val x = 0;
      |	  while (x < width)
      |			val c = toInt((y / height) * 3.0);
      |			val col = 0;
      |			if (c < 3) then
      |			   col <- colors[c];
      |			else
      |			   col <- 1;
      |			end;
      |			val dens = clamp(toInt((y / height) * 10.0), 9);
      |			putpixel(x, y, col, dens);
      |	  		x <- x + 1;
      |	  end;
      |	  y <- y + 1;
      |    end;
      |
      |	val sy = height - 15;
      |	val sx = (width / 2) - 10;
      |	val i = 0;
      |	val j = 0;
      |	while (i < 10)
      |		  j <- 0;
      |		  while (j < 20)
      |		        val w = toInt(sin(i * (pi / 10.0)) * 10.0);
      |				if (j < w) then
      |    				putpixel(sx + 10 + j, sy + i, 4, 9);
      |    				putpixel(sx + 10 - j, sy + i, 4, 9);
      |    			else
      |				    w <- w;
      |				end;
      |		  		j <- j + 1;
      |		  end;
      |		  i <- i + 1;
      |	end;
      |end;
      |
      |def scene4(N: int, sinTable: [double]): unit
      |    cls();
      |    val y = 0;
      |    while (y < height)
      |	  val x = 0;
      |	  while (x < width)
      |			val cbase = toInt((x * sin(x * (pi / 10.0))) + (y * cos(y * (pi / 10.0))) + (y * 4.0));
      |			val c = cbase mod 25;
      |			val col = colors[c / 10];
      |			val dens = c / 3;
      |			putpixel(x, y, col, dens);
      |	  		x <- x + 1;
      |	  end;
      |	  y <- y + 1;
      |    end;
      |end;
      |
      |def calcSinTable(res: int, scale: double): [double]
      |	val i = 0;
      |	array[double] sinTable;
      |	while (i < res)
      |		  sinTable[i] <- scale * sin(((2 * pi) / res) * i);
      |		  i <- i + 1;
      |    end;
      |
      |	return sinTable;
      |end;
      |
      |print("Precalculating....");
      |val N = 1000;
      |val sinTable = calcSinTable(N, 10.0);
      |
      |hideCursor();
      |cls();
      |
      |scene1(N, sinTable);
      |waitkey();
      |scene2(N, sinTable);
      |waitkey();
      |scene3(N, sinTable);
      |waitkey();
      |scene4(N, sinTable);
      |waitkey();
    """.stripMargin

  println("Gradient:")
  val gradientTokens = Parser.BarlangLexer.parse(Parser.BarlangLexer.tokens, gradientSrc).get
//  println(s"Tokens:")
//  gradientTokens.foreach(t => println(s"${t.pos.line}:${t.pos.column} $t"))

  val gradientResult = Parser.BarlangParser(gradientTokens)
  gradientResult match {
    case Parser.BarlangParser.Success(ast, next) =>
      println(ast.body.mkString("\n"))
      println("===")
      println(PrettyPrinter(ast))
    case Parser.BarlangParser.NoSuccess(msg, next) =>
      println(s"${next.pos.line}:${next.pos.column} $msg")
      println(gradientSrc.lines.toVector(next.pos.line-1))
  }

}
