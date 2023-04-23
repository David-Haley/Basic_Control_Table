-- Package to provided type declarations for Basic_CT.

-- Author    : David Haley
-- Created   : 26/03/2023
-- Last Edit : 23/04/2023
-- 20230423 : Track_Stores and Sub_Route_Lists changed from vector to doubly
-- linked list.

with Ada.Containers; use Ada.Containers;

package body CT_Types is

   function "<" (Left, Right : Track_Keys) return Boolean is
     (Left.Track_Name < Right.Track_Name or (Left.Track_Name = Right.Track_Name
      and Left.Track_End < Right.Track_End));

end CT_Types;
