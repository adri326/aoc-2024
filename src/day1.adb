with Ada.Text_IO; use Ada.Text_IO;
with Split_Line;
with Integer_Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;

procedure Day1 is
    F : File_Type;
    File_Name : constant String := "./input/day1.txt";

    Sum : Natural := 0;
    Current_Values : Integer_Vectors.Vector;
    Left_Values : Integer_Vectors.Vector;
    Right_Values : Integer_Vectors.Vector;

    package ISort is new Integer_Vectors.Generic_Sorting;
    package Integer_Count is new Ada.Containers.Indefinite_Hashed_Maps(
        Key_Type => Integer,
        Element_Type => Natural,
        Hash => Ada.Containers.Hash_Type'Mod,
        Equivalent_Keys => "="
    );

    Right_Counts : Integer_Count.Map;
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
        if Integer_Count.Contains(Right_Counts, Right_Values(I)) then
            Integer_Count.Replace(Right_Counts, Right_Values(I), Right_Counts(Right_Values(I)) + 1);
        else
            Integer_Count.Insert(Right_Counts, Right_Values(I), 1);
        end if;
    end loop;
    Put_Line(Sum'Image);

    Sum := 0;
    for L of Left_Values loop
        If Integer_Count.Contains(Right_Counts, L) then
            Sum := Sum + L * Integer_Count.Element(Right_Counts, L);
        end if;
    end loop;
    Put_Line(Sum'Image);
end Day1;
