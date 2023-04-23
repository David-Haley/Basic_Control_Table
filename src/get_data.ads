-- Package to read input data for Basic_CT.

-- Author    : David Haley
-- Created   : 25/03/2023
-- Last Edit : 23/04/2023
-- 20230423 :Track_Stores and Sub_Route_Lists changed from vector to doubly
-- linked list.

with CT_Types; use CT_Types;

package Get_Data is

   procedure Get (Track_Store : out Track_Stores.List);

   procedure Get (Signal_Store : out Signal_Stores.Map);

   procedure Get (Route_Store : out Route_Stores.Map);

end Get_Data;
