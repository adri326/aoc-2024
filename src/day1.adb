with Ada.Text_IO; use Ada.Text_IO;
with Split_Line;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Day1 is
    F : File_Type;
    File_Name : constant String := "./input/day1-example.txt";

    Sum : Integer := 0;
    Current_Values : Split_Line.Integer_Vectors.Vector;
    Test: Unbounded_String;
begin
    Open(F, In_File, File_Name);
    while not End_Of_File(F) loop
        Test := To_Unbounded_String(Source => Get_Line(F));
        Put_Line(To_String (Test));
        Current_Values := Split_Line.Split(To_String(Test));
        Sum := Sum + abs (Current_Values(0) - Current_Values(1));
    end loop;
    Put_Line(Sum'Image);
end Day1;
