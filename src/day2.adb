with Ada.Text_IO; use Ada.Text_IO;
with Grid; use Grid;
with Integer_Vectors;
with Ada.Containers; use Ada.Containers;

procedure Day2 is
   Input : Integer_Grid := Grid.Load("./input/day2.txt");

   subtype Integer_Vector is Integer_Vectors.Vector;

   function Is_Monotone (Row: Integer_Vector) return Boolean is
      Increasing : Boolean;
   begin
      if Integer_Vectors.Length(Row) <= 1 then
         return True;
      end if;

      if Row(1) > Row(0) then
         -- Increasing
         for I in 1 .. Natural(Integer_Vectors.Length(Row) - 1) loop
            if Row(I) <= Row(I - 1) then
               return False;
            end if;
         end loop;
      else
         -- Decreasing
         for I in 1 .. Natural(Integer_Vectors.Length(Row) - 1) loop
            if Row(I) >= Row(I - 1) then
               return False;
            end if;
         end loop;
      end if;

      return True;
   end;

   function Is_Within_Range (Row: Integer_Vector) return Boolean is
   begin
      if Integer_Vectors.Length(Row) <= 1 then
         return True;
      end if;

      for I in 1 .. Natural(Integer_Vectors.Length(Row) - 1) loop
         if abs (Row(I) - Row(I - 1)) > 3 then
            return False;
         end if;
      end loop;
      return True;
   end;

   function Is_Safe (Row: Integer_Vector) return Boolean is
   begin
      return Is_Monotone(Row) and Is_Within_Range(Row);
   end;


   function Is_Almost_Safe (Row: Integer_Vector) return Boolean is
      Row_Copy : Integer_Vector;
   begin
      if Is_Monotone(Row) and Is_Within_Range(Row) then
         return True;
      end if;

      for I in 0 .. Natural(Integer_Vectors.Length(Row) - 1) loop
         Row_Copy := Row.Copy;
         Row_Copy.Delete(I);


         if Is_Monotone(Row_Copy) and Is_Within_Range(Row_Copy) then
            return True;
         end if;
      end loop;

      return False;
   end;

   Safe_Count : Natural := 0;
   Almost_Safe_Count : Natural := 0;
begin
   for Row of Input loop
      if Is_Safe(Row) then
         Safe_Count := Safe_Count + 1;
         Almost_Safe_Count := Almost_Safe_Count + 1;
      elsif Is_Almost_Safe(Row) then
         Almost_Safe_Count := Almost_Safe_Count + 1;
      end if;
   end loop;

   Put_Line(Safe_Count'Image);
   Put_Line(Almost_Safe_Count'Image);
end;
