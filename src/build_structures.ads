-- Package to build data structures.

-- Author    : David Haley
-- Created   : 27/03/2023
-- Last Edit : 10/04/2023
-- 20230410 : Building of points data added.

with CT_Types; use CT_Types;

package Build_Structures is

   procedure Build (Track_Store : in Track_Stores.Vector;
                    Track_Dictionary : out Track_Dictionaries.Map);
   -- Builds an ordered list of all track entereces.

   procedure Build (Signal_Store : in Signal_Stores.Map;
                    Track_Dictionary : in Track_Dictionaries.Map;
                    Sub_Route_to_Signal_Map : out Sub_Route_to_Signal_Maps.Map);
   -- Builds a list of signals indexed by their replacement tracks;

   procedure Build (Track_Store : in Track_Stores.Vector;
                    Signal_Store : in Signal_Stores.Map;
                    Route_Store : in Route_Stores.Map;
                    Track_Dictionary : in Track_Dictionaries.Map;
                    Sub_Route_to_Signal_Map : in Sub_Route_to_Signal_Maps.Map;
                    Route_Map : out Route_Maps.Map);
   -- Builds a list of all the in-route tracks forming a route starting from
   -- the entrance signal and ending with the track before the exit signal
   -- replacement track.

   procedure Build (Track_Store : in Track_Stores.Vector;
                    Track_Dictionary : in Track_Dictionaries.Map;
                    Sub_Route_List : in Sub_Route_Lists.Vector;
                    Point_List : out Point_Lists.Vector);
   -- Builds the points related data for a single route as defined by the
   -- Sub_Route list. Each points number should appear once in the list even if
   -- another set of points intervenes, may be possible for remote trap points.

end Build_Structures;
