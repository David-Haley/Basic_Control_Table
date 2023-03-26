-- Program to create a basic control table from track layout information input
-- as CSV tables. This is likely to be subject to scope creep but initially
-- in-route signal to signal and signal to points locking is targeted as being
-- Most critical to safety and least variable in terms of signalling rules.

-- Author    : David Haley
-- Created   : 24/03/2023
-- Last Edit : 26/03/2023

with Ada.Text_IO; use Ada.Text_IO;
with CT_Types; use CT_Types;
with Get_Data; use Get_Data;

procedure Basic_Ct is

   Track_Store : Track_Stores.Vector;
   Signal_Store : Signal_Stores.Map;
   Route_Store : Route_Stores.Map;

begin
   Put_Line ("Basic Control Table 20230326");
   Get (Track_Store);
   Get (Signal_Store);
   Get (Route_Store);
end Basic_Ct;
