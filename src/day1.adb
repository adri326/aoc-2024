with Ada.Text_IO; use Ada.Text_IO;
with Split_Line;
with Integer_Vectors;

procedure Day1 is
    F : File_Type;
    File_Name : constant String := "./input/day1.txt";

    Sum : Natural := 0;
    Current_Values : Integer_Vectors.Vector;
    Left_Values : Integer_Vectors.Vector;
    Right_Values : Integer_Vectors.Vector;
    I : Natural := 0;

    package ISort is new Integer_Vectors.Generic_Sorting;
begin
    Open(F, In_File, File_Name);
    while not End_Of_File(F) loop
        Current_Values := Split_Line.Split(Get_Line(F));
        Left_Values.Append(Current_Values(0));
        Right_Values.Append(Current_Values(1));
    end loop;
    ISort.Sort(Left_Values);
    ISort.Sort(Right_Values);

    for I in 0 .. Natural(Left_Values.Length) - 1 loop
        Sum := Sum + abs (Left_Values(I) - Right_Values(I));
    end loop;
    Put_Line(Sum'Image);
end Day1;
