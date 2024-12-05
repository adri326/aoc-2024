with Ada.Text_IO; use Ada.Text_IO;
with Split_Line;
with Integer_Vectors;
use Integer_Vectors;
with Ada.Containers; use Ada.Containers;

package body Grid is
   function Load (Path: String) return Integer_Semigrid is
      F : File_Type;
      Result : Integer_Semigrid;

      Current_Line : Integer_Vectors.Vector;
   begin
      Open(F, In_File, Path);
      while not End_Of_File(F) loop
         Current_Line := Split_Line.Split(Get_Line(F));
         Result.Append(Current_Line);
      end loop;
      return Result;
   end Load;

   function To_Grid (Semi: Integer_Semigrid) return Integer_Grid is
      Result : Integer_Grid;
      type Width_Type is range -1 .. 2**32 - 2;
      Width : Width_Type := -1;

      Current_Width : Width_Type;
   begin
      for Row of Semi loop
         Current_Width := Width_Type(Integer_Vectors.Length(Row));
         if Width = -1 or Current_Width = Width then
            Width := Current_Width;
            Result.Append(Row);
         else
            raise Dimension_Except;
         end if;
      end loop;
      return Result;
   end To_Grid;

   function To_Semigrid (G: Integer_Grid) return Integer_Semigrid is
      Result : Integer_Semigrid;
   begin
      for Row of G loop
         Result.Append(Row);
      end loop;
      return Result;
   end To_Semigrid;

   procedure Print (G: Integer_Semigrid) is
      First : Boolean;
   begin
      if Integer_Grids.Length(G) = 0 then
         Put_Line("Dims: 0x0");
      else
         Put_Line("Dims: " & Integer_Vectors.Length(G(0))'Image & "x" & Integer_Grids.Length(G)'Image);
      end if;

      for Row of G loop
         First := True;
         for Item of Row loop
            if First then
               Put(Item'Image);
            else
               Put("," & Item'Image);
            end if;
            First := False;
         end loop;
         Put_Line("");
      end loop;
   end Print;
end Grid;
