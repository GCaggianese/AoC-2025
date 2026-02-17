--  SPDX-FileCopyrightText: 2026 Germán Caggianese <german.caggianese@pm.me>
--
--  SPDX-License-Identifier: Apache-2.0

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Aoc_D8 is

   type Point is record
      X, Y, Z : Integer;
   end record;

   package Point_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Point);

   type Edge is record
      Distance : Float;
      Box_A    : Natural;
      Box_B    : Natural;
   end record;

   package Edge_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Edge);

   function "<" (Left, Right : Edge) return Boolean is
   begin
      return Left.Distance < Right.Distance;
   end "<";

   package Edge_Sorting is new Edge_Vectors.Generic_Sorting;

   Points : Point_Vectors.Vector;
   Edges  : Edge_Vectors.Vector;
   File   : File_Type;

   --  Union-Find structure
   type Parent_Array is array (Natural range <>) of Natural;

   function Find (Parent : in out Parent_Array; X : Natural) return Natural is
   begin
      if Parent (X) = X then
         return X;
      else
         --  Path compression: make X point directly to root
         Parent (X) := Find (Parent, Parent (X));
         return Parent (X);
      end if;
   end Find;

   procedure Union (Parent : in out Parent_Array; X, Y : Natural) is
      Root_X : constant Natural := Find (Parent, X);
      Root_Y : constant Natural := Find (Parent, Y);
   begin
      if Root_X /= Root_Y then
         Parent (Root_X) := Root_Y;
      end if;
   end Union;

   function Parse_Point (Line : String) return Point is
      First_Comma  : Natural;
      Second_Comma : Natural;
      P            : Point;
   begin
      First_Comma  := Index (Line, ",");
      Second_Comma := Index (Line, ",", First_Comma + 1);

      P.X := Integer'Value (Line (Line'First .. First_Comma - 1));
      P.Y := Integer'Value (Line (First_Comma + 1 .. Second_Comma - 1));
      P.Z := Integer'Value (Line (Second_Comma + 1 .. Line'Last));

      return P;
   end Parse_Point;

   function Distance (A, B : Point) return Float is
      DX : constant Float := Float (A.X - B.X);
      DY : constant Float := Float (A.Y - B.Y);
      DZ : constant Float := Float (A.Z - B.Z);
   begin
      return Sqrt (DX * DX + DY * DY + DZ * DZ);
   end Distance;

begin
   --  Open and read file
   Open (File => File, Mode => In_File, Name => "input.txt");

   while not End_Of_File (File) loop
      declare
         Line : constant String := Get_Line (File);
         P    : Point;
      begin
         P := Parse_Point (Line);
         Points.Append (P);
      end;
   end loop;

   Close (File);

   Put_Line ("Total points read:" & Points.Length'Image);

   --  Generate all pairs
   Put_Line ("Computing distances...");

   for I in 0 .. Natural (Points.Length) - 1 loop
      for J in I + 1 .. Natural (Points.Length) - 1 loop
         declare
            E : Edge;
         begin
            E.Distance := Distance (Points.Element (I), Points.Element (J));
            E.Box_A    := I;
            E.Box_B    := J;
            Edges.Append (E);
         end;
      end loop;
   end loop;

   Put_Line ("Total edges:" & Edges.Length'Image);

   --  Sort edges
   Put_Line ("Sorting edges...");
   Edge_Sorting.Sort (Edges);

   --  Initialize Union-Find
   declare
      N               : constant Natural := Natural (Points.Length);
      Parent          : Parent_Array (0 .. N - 1);
      Num_Circuits    : Natural          := N;
      Edges_Processed : Natural          := 0;
      Part1_Done      : Boolean          := False;
      Last_Edge       : Edge;

      --  For counting circuit sizes (Part 1)
      package Natural_Vectors is new Ada.Containers.Vectors
        (Index_Type => Natural, Element_Type => Natural);
      use Ada.Containers;
   begin
      --  Each box starts in its own circuit
      for I in Parent'Range loop
         Parent (I) := I;
      end loop;

      Put_Line ("Processing edges...");

      --  Process all edges until everything is connected
      for E of Edges loop
         Edges_Processed := Edges_Processed + 1;

         if Find (Parent, E.Box_A) /= Find (Parent, E.Box_B) then
            --  Different circuits - merge them
            Union (Parent, E.Box_A, E.Box_B);
            Num_Circuits := Num_Circuits - 1;
         end if;

         --  === PART 1: After processing 1000 edges ===
         if Edges_Processed = 1_000 and then not Part1_Done then
            Put_Line ("");
            Put_Line ("=== PART 1: After 1000 edges ===");

            declare
               Circuit_Sizes  : Natural_Vectors.Vector;
               Circuit_Counts : array (0 .. N - 1) of Natural := (others => 0);
            begin
               --  Count how many boxes belong to each circuit root
               for I in 0 .. N - 1 loop
                  declare
                     Root : constant Natural := Find (Parent, I);
                  begin
                     Circuit_Counts (Root) := Circuit_Counts (Root) + 1;
                  end;
               end loop;

               --  Collect non-zero circuit sizes
               for Size of Circuit_Counts loop
                  if Size > 0 then
                     Circuit_Sizes.Append (Size);
                  end if;
               end loop;

               Put_Line ("Number of circuits:" & Circuit_Sizes.Length'Image);

               --  Sort circuit sizes (ascending)
               declare
                  package Size_Sorting is new Natural_Vectors.Generic_Sorting;
               begin
                  Size_Sorting.Sort (Circuit_Sizes);
               end;

               --  Calculate answer: multiply 3 largest
               if Natural (Circuit_Sizes.Length) >= 3 then
                  declare
                     Len : constant Natural := Natural (Circuit_Sizes.Length);
                     Size1  : constant Natural :=
                       Circuit_Sizes.Element (Len - 1);
                     Size2  : constant Natural :=
                       Circuit_Sizes.Element (Len - 2);
                     Size3  : constant Natural :=
                       Circuit_Sizes.Element (Len - 3);
                     Answer : constant Natural := Size1 * Size2 * Size3;
                  begin
                     Put_Line
                       ("Three largest circuits:" & Size1'Image & " *" &
                        Size2'Image & " *" & Size3'Image & " =" &
                        Answer'Image);
                  end;
               end if;
            end;

            Part1_Done := True;
            Put_Line ("Continuing to Part 2...");
            Put_Line ("");
         end if;

         --  === PART 2: When all boxes are in one circuit ===
         if Num_Circuits = 1 then
            Last_Edge := E;

            Put_Line ("=== PART 2: All boxes connected ===");
            Put_Line ("Total edges processed:" & Edges_Processed'Image);

            declare
               Box_A  : constant Point   := Points.Element (Last_Edge.Box_A);
               Box_B  : constant Point   := Points.Element (Last_Edge.Box_B);
               Answer : constant Integer := Box_A.X * Box_B.X;
            begin
               Put_Line
                 ("Last connection: boxes" & Last_Edge.Box_A'Image & " and" &
                  Last_Edge.Box_B'Image);
               Put_Line
                 ("Box A: (" & Box_A.X'Image & "," & Box_A.Y'Image & "," &
                  Box_A.Z'Image & ")");
               Put_Line
                 ("Box B: (" & Box_B.X'Image & "," & Box_B.Y'Image & "," &
                  Box_B.Z'Image & ")");
               Put_Line
                 ("X coordinates:" & Box_A.X'Image & " *" & Box_B.X'Image);
               Put_Line ("Part 2 Answer:" & Answer'Image);
            end;

            exit;  --  Done!
         end if;
      end loop;
   end;

end Aoc_D8;
