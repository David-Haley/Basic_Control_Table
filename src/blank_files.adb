-- This program creates empty CSV files with the appropriate to be used as input
-- to Basic_CT.

-- Author    : David Haley
-- Created   : 27/03/2023
-- Last Edit : 27/03/2023

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories; use Ada.Directories;
with Ada.Command_Line; use Ada.Command_Line;
with File_Heders; use File_Heders;

procedure Blank_Files is

   function Title_Case (In_String : in String) return String is

      Result : String := In_String;

   begin -- Title_Case
      for I in Positive range Result'First .. Result'Last loop
         if I = Result'First or else Result (I - 1) = Low_Line then
            Result (I) := To_Upper (Result (I));
         else
            Result (I) := To_Lower (Result (I));
         end if; -- I = Result'First or else Result (I - 1) = Low_Line
      end loop; -- I in Positive range Result'First .. Result'Last
      return '"' & Result & '"';
   end Title_Case;

   Working_Directory : Unbounded_String := Null_Unbounded_String;
   Output_File : File_Type;

begin -- Blank_Files
   Put_Line ("Blank_Files version 20230327");
   if Argument_Count = 1 then
      Working_Directory := To_Unbounded_String (Argument (1));
   end if; -- Argument_Count = 1
   Create (Output_File, Out_File,
           Compose (To_String (Working_Directory), Track_File_Name, CSV));
   Put_Line ("Writing " & Name (Output_File));
   for H in Track_Header loop
      Put (Output_File, Title_Case (H'Img));
      if H = Track_Header'Last then
         New_Line (Output_File);
      else
         Put (Output_File, Comma);
      end if; -- I = Track_Header'Last
   end loop; -- H in Track_Header
   Close (Output_File);
   Create (Output_File, Out_File,
           Compose (To_String (Working_Directory), Points_File_Name, CSV));
   Put_Line ("Writing " & Name (Output_File));
   for H in Points_Header loop
      Put (Output_File, Title_Case (H'Img));
      if H = Points_Header'Last then
         New_Line (Output_File);
      else
         Put (Output_File, Comma);
      end if; -- H = Points_Header'Last
   end loop; -- H in Points_Header
   Close (Output_File);
   Create (Output_File, Out_File,
           Compose (To_String (Working_Directory), Diamond_File_Name, CSV));
   Put_Line ("Writing " & Name (Output_File));
   for H in Diamond_Header loop
      Put (Output_File, Title_Case (H'Img));
      if H = Diamond_Header'Last then
         New_Line (Output_File);
      else
         Put (Output_File, Comma);
      end if; -- H = Diamond_Header'Last
   end loop; -- H in Diamond_Header
   Close (Output_File);
   Create (Output_File, Out_File,
           Compose (To_String (Working_Directory), Switch_Diamond_File_Name, CSV));
   Put_Line ("Writing " & Name (Output_File));
   for H in Switch_Diamond_Header loop
      Put (Output_File, Title_Case (H'Img));
      if H = Switch_Diamond_Header'Last then
         New_Line (Output_File);
      else
         Put (Output_File, Comma);
      end if; -- H = Switch_Diamond_Header'Last
   end loop; -- H in Switch_Diamond_Header
   Close (Output_File);
   Create (Output_File, Out_File,
           Compose (To_String (Working_Directory), Signal_File_Name, CSV));
   Put_Line ("Writing " & Name (Output_File));
   for H in Signal_Header loop
      Put (Output_File, Title_Case (H'Img));
      if H = Signal_Header'Last then
         New_Line (Output_File);
      else
         Put (Output_File, Comma);
      end if; -- H = Signal_Header'Last
   end loop; -- H in Signal_Header
   Close (Output_File);
   Create (Output_File, Out_File,
           Compose (To_String (Working_Directory), Route_File_Name, CSV));
   Put_Line ("Writing " & Name (Output_File));
   for H in Route_Header loop
      Put (Output_File, Title_Case (H'Img));
      if H = Route_Header'Last then
         New_Line (Output_File);
      else
         Put (Output_File, Comma);
      end if; -- H = Route_Header'Last
   end loop; -- H in Route_Header
   Close (Output_File);
   Put_Line ("File creation complete");
end Blank_Files;
