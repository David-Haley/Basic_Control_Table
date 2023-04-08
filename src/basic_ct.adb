-- Program to create a basic control table from track layout information input
-- as CSV tables. This is likely to be subject to scope creep but initially
-- in-route signal to signal and signal to points locking is targeted as being
-- Most critical to safety and least variable in terms of signalling rules.

-- Author    : David Haley
-- Created   : 24/03/2023
-- Last Edit : 08/04/2023

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with CT_Types; use CT_Types;
with Get_Data; use Get_Data;
with Build_Structures; use Build_Structures;
use CT_Types.Route_Maps;
use CT_Types.Sub_Route_Lists;

procedure Basic_Ct is

   Track_Store : Track_Stores.Vector;
   Signal_Store : Signal_Stores.Map;
   Route_Store : Route_Stores.Map;
   Track_Dictionary : Track_Dictionaries.Map;
   Sub_Route_to_Signal_Map : Sub_Route_to_Signal_Maps.Map;
   Route_Map : Route_Maps.Map;
   Output_File : File_Type;

begin
   Put_Line ("Basic Control Table 20230402");
   Get (Track_Store);
   Get (Signal_Store);
   Get (Route_Store);
   Build (Track_Store, Track_Dictionary);
   Build (Signal_Store, Track_Dictionary, Sub_Route_to_Signal_Map);
   Build (Track_Store, Signal_Store, Route_Store, Track_Dictionary,
          Sub_Route_to_Signal_Map, Route_Map);
   Create (Output_File, Out_File, "Basic_CT.txt");
   for R in Iterate (Route_Map) loop
      Put (Output_File,  Key (R));
      Put (Output_File, " {");
      for S in Iterate (Route_Map (R)) loop
         Put (Output_File, Route_Map (R) (S).Track_Name);
         Put (Output_File, Route_Map (R) (S).Entrance_End);
         Put (Output_File, Route_Map (R) (S).Exit_End);
         if S /= Last (Route_Map (R)) then
            Put (Output_File, ", ");
         else
            Put_line (Output_File, "}");
         end if; -- S /= Last (Route_Map (R))
      end loop; -- S in Iterate (Route_Map (R))
   end loop; -- R in Iterate (Route_Map)
end Basic_Ct;
