with Hera.Game;
with Hera.Json;

package Hera.UI.Web_UI.Requests is

   function Game_State
     (Game : Hera.Game.Game_Type)
      return Hera.Json.Json_Value'Class;

   function Current_Phase
     (Game : Hera.Game.Game_Type)
      return Hera.Json.Json_Value'Class;

   function Faction_Names
     (Game : Hera.Game.Game_Type)
      return Hera.Json.Json_Value'Class;

   function Faction_State
     (Game : Hera.Game.Game_Type)
      return Hera.Json.Json_Value'Class;

   function Fleet_State
     (Game : Hera.Game.Game_Type)
      return Hera.Json.Json_Value'Class;

   function Legion_State
     (Game : Hera.Game.Game_Type)
      return Hera.Json.Json_Value'Class;

   function Republic_State
     (Game : Hera.Game.Game_Type)
      return Hera.Json.Json_Value'Class;

   function Continue
     (Game : in out Hera.Game.Game_Type)
      return Hera.Json.Json_Value'Class;

end Hera.UI.Web_UI.Requests;
