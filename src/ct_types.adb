-- Package to provided type declarations for Basic_CT.

-- Author    : David Haley
-- Created   : 26/03/2023
-- Last Edit : 25/04/2023
-- 20230425 : Route_Sets and Conflict_Maps added.
-- 20230423 : Track_Stores and Sub_Route_Lists changed from vector to doubly
-- linked list.

with Ada.Containers; use Ada.Containers;

package body CT_Types is

   function "<" (Left, Right : Track_Keys) return Boolean is
     (Left.Track_Name < Right.Track_Name or (Left.Track_Name = Right.Track_Name
      and Left.Track_End < Right.Track_End));

   function "<" (Left, Right : Sub_Routes) return Boolean is
     (Left.Track_Name < Right.Track_Name or
        (Left.Track_Name = Right.Track_Name and
             (Left.Entrance_End < Right.Entrance_End or
                  (Left.Entrance_End = Right.Entrance_End and
                         Left.Exit_End < Right.Exit_End))));

end CT_Types;
