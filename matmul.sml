(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Array2New : ARRAY2 =
   struct
      infix 6 +! -!
      infix 7 *!
      structure SeqIndex = struct
        open Int
        val op +! = Int.+
        val op -! = Int.-
        val op *! = Int.*
        val ltu = Int.<
        val leu = Int.<=
        val gtu = Int.>
        val geu = Int.>=
        fun toIntUnsafe x = x
        fun fromIntUnsafe x = x
      end
      structure Primitive = struct
        structure Controls = struct
          val safe = true
        end
        structure Array = struct
          open Array
          val unsafeUpdate = update
          val unsafeSub = sub
          val new = array
          fun alloc i sz = array(sz,i)
          fun array0 () : 'a array = Array.tabulate(0,fn _ => raise Fail "impossible")
          structure Slice = ArraySlice
        end
        structure Vector = Vector
      end
      val op +! = SeqIndex.+!
      val op + = SeqIndex.+
      val op -! = SeqIndex.-!
      val op - = SeqIndex.-
      val op *! = SeqIndex.*!
      val op * = SeqIndex.*
      val op < = SeqIndex.<
      val op <= = SeqIndex.<=
      val op > = SeqIndex.>
      val op >= = SeqIndex.>=
      val ltu = SeqIndex.ltu
      val leu = SeqIndex.leu
      val gtu = SeqIndex.gtu
      val geu = SeqIndex.geu

      type 'a array = {array: 'a Array.array,
                       rows: SeqIndex.int,
                       cols: SeqIndex.int}

      fun dimensions' ({rows, cols, ...}: 'a array) = (rows, cols)
      fun dimensions ({rows, cols, ...}: 'a array) =
         (SeqIndex.toIntUnsafe rows, SeqIndex.toIntUnsafe cols)
      fun nRows' ({rows, ...}: 'a array) = rows
      fun nRows ({rows, ...}: 'a array) = SeqIndex.toIntUnsafe rows
      fun nCols' ({cols, ...}: 'a array) = cols
      fun nCols ({cols, ...}: 'a array) = SeqIndex.toIntUnsafe cols

      type 'a region = {base: 'a array,
                        row: int,
                        col: int,
                        nrows: int option,
                        ncols: int option}

      local
         fun checkSliceMax' (start: int,
                             num: SeqIndex.int option,
                             max: SeqIndex.int): SeqIndex.int * SeqIndex.int =
            case num of
               NONE => if Primitive.Controls.safe
                          then let
                                  val start =
                                     (SeqIndex.fromInt start)
                                     handle Overflow => raise Subscript
                               in
                                  if gtu (start, max)
                                     then raise Subscript
                                     else (start, max)
                               end
                          else (SeqIndex.fromIntUnsafe start, max)
             | SOME num => if Primitive.Controls.safe
                              then let
                                      val start =
                                         (SeqIndex.fromInt start)
                                         handle Overflow => raise Subscript
                                   in
                                      if (start < 0 orelse num < 0
                                          orelse start +! num > max)
                                         then raise Subscript
                                         else (start, start +! num)
                                   end
                              else (SeqIndex.fromIntUnsafe start,
                                    SeqIndex.fromIntUnsafe start +! num)
         fun checkSliceMax (start: int,
                            num: int option,
                            max: SeqIndex.int): SeqIndex.int * SeqIndex.int =
            if Primitive.Controls.safe
               then (checkSliceMax' (start, Option.map SeqIndex.fromInt num, max))
                    handle Overflow => raise Subscript
               else checkSliceMax' (start, Option.map SeqIndex.fromIntUnsafe num, max)
      in
         fun checkRegion' {base, row, col, nrows, ncols} =
            let
               val (rows, cols) = dimensions' base
               val (startRow, stopRow) = checkSliceMax' (row, nrows, rows)
               val (startCol, stopCol) = checkSliceMax' (col, ncols, cols)
            in
               {startRow = startRow, stopRow = stopRow,
                startCol = startCol, stopCol = stopCol}
            end
         fun checkRegion {base, row, col, nrows, ncols} =
            let
               val (rows, cols) = dimensions' base
               val (startRow, stopRow) = checkSliceMax (row, nrows, rows)
               val (startCol, stopCol) = checkSliceMax (col, ncols, cols)
            in
               {startRow = startRow, stopRow = stopRow,
                startCol = startCol, stopCol = stopCol}
            end
      end

      fun wholeRegion (a : 'a array): 'a region =
         {base = a, row = 0, col = 0, nrows = NONE, ncols = NONE}

      datatype traversal = RowMajor | ColMajor

      local
         fun make (rows, cols, doit) =
            if Primitive.Controls.safe
               andalso (rows < 0 orelse cols < 0)
               then raise Size
            else {array = doit (rows * cols handle Overflow => raise Size),
                  rows = rows,
                  cols = cols}
      in
         fun alloc' (rows, cols, init) =
            make (rows, cols, Primitive.Array.alloc init)
         fun array' (rows, cols, init) =
            make (rows, cols, fn size => Primitive.Array.new (size, init))
      end
      local
         fun make (rows, cols, doit) =
            if Primitive.Controls.safe
               then let
                       val rows =
                          (SeqIndex.fromInt rows)
                          handle Overflow => raise Size
                       val cols =
                          (SeqIndex.fromInt cols)
                          handle Overflow => raise Size
                    in
                       doit (rows, cols)
                    end
               else doit (SeqIndex.fromIntUnsafe rows,
                          SeqIndex.fromIntUnsafe cols)
      in
         fun alloc (rows, cols, init) =
            make (rows, cols, fn (rows, cols) => alloc' (rows, cols, init))
         fun array (rows, cols, init) =
            make (rows, cols, fn (rows, cols) => array' (rows, cols, init))
      end

      fun array0 (): 'a array =
         {array = Primitive.Array.array0(),
          rows = 0,
          cols = 0}

      fun unsafeSpot' ({cols, ...}: 'a array, r, c) =
         r *! cols +! c
      fun spot' (a as {rows, cols, ...}: 'a array, r, c) =
         if Primitive.Controls.safe
            andalso (geu (r, rows) orelse geu (c, cols))
            then raise Subscript
            else unsafeSpot' (a, r, c)

      fun unsafeSub' (a as {array, ...}: 'a array, r, c) =
         Primitive.Array.unsafeSub (array, unsafeSpot' (a, r, c))
      fun sub' (a as {array, ...}: 'a array, r, c) =
         Primitive.Array.unsafeSub (array, spot' (a, r, c))
      fun unsafeUpdate' (a as {array, ...}: 'a array, r, c, x) =
         Primitive.Array.unsafeUpdate (array, unsafeSpot' (a, r, c), x)
      fun update' (a as {array, ...}: 'a array, r, c, x) =
         Primitive.Array.unsafeUpdate (array, spot' (a, r, c), x)

      local
         fun make (r, c, doit) =
            if Primitive.Controls.safe
               then let
                       val r =
                          (SeqIndex.fromInt r)
                          handle Overflow => raise Subscript
                       val c =
                          (SeqIndex.fromInt c)
                          handle Overflow => raise Subscript
                    in
                       doit (r, c)
                    end
               else doit (SeqIndex.fromIntUnsafe r,
                          SeqIndex.fromIntUnsafe c)
      in
         fun sub (a, r, c) =
            make (r, c, fn (r, c) => sub' (a, r, c))
         fun update (a, r, c, x) =
            make (r, c, fn (r, c) => update' (a, r, c, x))
      end

      fun 'a fromList (rows: 'a list list): 'a array =
         case rows of
            [] => array0 ()
          | row1 :: _ =>
               let
                  val cols = length row1
                  val a as {array, cols = cols', ...} =
                     if cols=0 then array0() else alloc (length rows, cols, List.hd row1)
                  val _ =
                     List.foldl
                     (fn (row: 'a list, i) =>
                      let
                         val max = i +! cols'
                         val i' =
                            List.foldl (fn (x: 'a, i) =>
                                        (if i >= max
                                            then raise Size
                                         else (Primitive.Array.unsafeUpdate (array, i, x)
                                               ; i +! 1)))
                            i row
                      in if i' = max
                            then i'
                         else raise Size
                      end)
                     0 rows
               in
                  a
               end

      fun row' ({array, rows, cols}, r) =
         if Primitive.Controls.safe andalso geu (r, rows)
            then raise Subscript
         else
            ArraySlice.vector (Primitive.Array.Slice.slice (array, r *! cols, SOME cols))
      fun row (a, r) =
         if Primitive.Controls.safe
            then let
                    val r =
                       (SeqIndex.fromInt r)
                       handle Overflow => raise Subscript
                 in
                    row' (a, r)
                 end
            else row' (a, SeqIndex.fromIntUnsafe r)
      fun column' (a as {rows, cols, ...}: 'a array, c) =
         if Primitive.Controls.safe andalso geu (c, cols)
            then raise Subscript
         else
            Primitive.Vector.tabulate (rows, fn r => unsafeSub' (a, r, c))
      fun column (a, c) =
         if Primitive.Controls.safe
            then let
                    val c =
                       (SeqIndex.fromInt c)
                       handle Overflow => raise Subscript
                 in
                    column' (a, c)
                 end
            else column' (a, SeqIndex.fromIntUnsafe c)

      fun foldi' trv f b (region as {base, ...}) =
         let
            val {startRow, stopRow, startCol, stopCol} = checkRegion region
         in
            case trv of
               RowMajor =>
                  let
                     fun loopRow (r, b) =
                        if r >= stopRow then b
                           else let
                                   fun loopCol (c, b) =
                                      if c >= stopCol then b
                                         else loopCol (c +! 1, f (r, c, sub' (base, r, c), b))
                                in
                                   loopRow (r +! 1, loopCol (startCol, b))
                                end
                  in
                     loopRow (startRow, b)
                  end
             | ColMajor =>
                  let
                     fun loopCol (c, b) =
                        if c >= stopCol then b
                           else let
                                   fun loopRow (r, b) =
                                      if r >= stopRow then b
                                         else loopRow (r +! 1, f (r, c, sub' (base, r, c), b))
                                in
                                   loopCol (c +! 1, loopRow (startRow, b))
                                end
                  in
                     loopCol (startCol, b)
                  end
         end

      fun foldi trv f b a =
         foldi' trv (fn (r, c, x, b) =>
                     f (SeqIndex.toIntUnsafe r,
                        SeqIndex.toIntUnsafe c,
                        x, b)) b a
      fun fold trv f b a =
          foldi trv (fn (_, _, x, b) => f (x, b)) b (wholeRegion a)

      fun appi trv f =
         foldi trv (fn (r, c, x, ()) => f (r, c, x)) ()

      fun app trv f = fold trv (f o #1) ()

      fun modifyi trv f (r as {base, ...}) =
         appi trv (fn (r, c, x) => update (base, r, c, f (r, c, x))) r

      fun modify trv f a = modifyi trv (f o #3) (wholeRegion a)

      fun tabulate trv (rows, cols, f) =
         let
            val a = if rows=0 orelse cols=0 then array0() else alloc (rows, cols, f(0,0))
            val () = modifyi trv (fn (r, c, _) => f (r, c)) (wholeRegion a)
         in
            a
         end

      fun copy {src = src as {base, ...}: 'a region,
                dst, dst_row, dst_col} =
         let
            val {startRow, stopRow, startCol, stopCol} = checkRegion src
            val nrows = stopRow -! startRow
            val ncols = stopCol -! startCol
            val {startRow = dst_row, startCol = dst_col, ...} =
               checkRegion' {base = dst, row = dst_row, col = dst_col,
                             nrows = SOME nrows,
                             ncols = SOME ncols}
            fun forUp (start, stop, f: SeqIndex.int -> unit) =
               let
                  fun loop i =
                     if i >= stop
                        then ()
                     else (f i; loop (i + 1))
               in loop start
               end
            fun forDown (start, stop, f: SeqIndex.int -> unit) =
               let
                  fun loop i =
                     if i < start
                        then ()
                     else (f i; loop (i - 1))
               in loop (stop -! 1)
               end
            val forRows = if startRow <= dst_row then forDown else forUp
            val forCols = if startCol <= dst_col then forUp else forDown
         in forRows (0, nrows, fn r =>
            forCols (0, ncols, fn c =>
                     unsafeUpdate' (dst, dst_row +! r, dst_col +! c,
                                    unsafeSub' (base, startRow +! r, startCol +! c))))
         end
   end

(* Written by Stephen Weeks (sweeks@acm.org). *)
structure Array = Array2

fun 'a fold (n : int, b : 'a, f : int * 'a -> 'a) =
   let
      fun loop (i : int, b : 'a) : 'a =
	 if i = n
	    then b
	 else loop (i + 1, f (i, b))
   in loop (0, b)
   end

fun foreach (n : int, f : int -> unit) : unit =
   fold (n, (), f o #1)

fun mult (a1 : real Array.array, a2 : real Array.array) : real Array.array =
   let
      val r1 = Array.nRows a1
      val c1 = Array.nCols a1
      val r2 = Array.nRows a2
      val c2 = Array.nCols a2
   in if c1 <> r2
	 then raise Fail "mult"
      else
	 let val a = Array2.array (r1, c2, 0.0)
	    fun dot (r, c) =
	       fold (c1, 0.0, fn (i, sum) =>
		    sum + Array.sub (a1, r, i) * Array.sub (a2, i, c))
	 in foreach (r1, fn r =>
		    foreach (c2, fn c =>
			    Array.update (a, r, c, dot (r,c))));
	    a
	 end
   end

structure Main =
   struct
      fun doit () =
	 let
	    val dim = 200
	    val a = Array.array (dim, dim, 1.0)
	 in if Real.== (200.0, Array2.sub (mult (a, a), 0, 0))
	       then print "Ok\n"
	    else raise Fail "bug"
	 end

      val doit =
	 fn () =>
	 let
	    fun loop n = if n = 0 then ()
	                 else (doit (); loop (n-1))
	 in loop 10
	 end
   end

val _ = Main.doit()
