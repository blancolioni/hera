with Hera.UI.Models.Data_Source;

with Hera.Knowledge;

with Hera.Corporations;
with Hera.Planets;

package body Hera.UI.Models.Planets is

   type Planet_Model_Type is new Root_Hera_Model with
      record
         Planet   : Hera.Planets.Planet_Type;
      end record;

   overriding procedure Start
     (Model     : in out Planet_Model_Type;
      User      : Hera.UI.UI_Account;
      Arguments : String);

   overriding function Name
     (Model : Planet_Model_Type)
      return String
   is ("planet-model");

   overriding function Default_View_Name
     (Model : Planet_Model_Type)
      return String
   is ("Planet");

   overriding function Changed
     (Model : Planet_Model_Type)
      return Boolean
   is (False);

   overriding function Handle
     (Model   : in out Planet_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class;

   overriding function Get
     (Model   : Planet_Model_Type;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Model   : Planet_Model_Type;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class
   is
      pragma Unreferenced (State, Client, Request);

      Result  : Json.Json_Object;
      Sectors : Json.Json_Array;

      procedure Serialize_Tile
        (Tile : Hera.Planets.Planet_Tile);

      --------------------
      -- Serialize_Tile --
      --------------------

      procedure Serialize_Tile
        (Tile : Hera.Planets.Planet_Tile)
      is
      begin
         Sectors.Append
           (Hera.Planets.Serialize (Tile));
      end Serialize_Tile;

   begin
      Result.Set_Property
        ("title", Model.Planet.Name);

      Model.Planet.Iterate_Tiles
        (Serialize_Tile'Access);

      Result.Set_Property ("sectors", Sectors);
      Result.Set_Property ("name", Model.Planet.Name);
      Result.Set_Property ("id", String (Model.Planet.Identifier));
      return Result;

   end Get;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Model   : in out Planet_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class
   is
   begin
      return Model.Get (State, Client, Request);
   end Handle;

   ------------------
   -- Planet_Model --
   ------------------

   function Planet_Model
     return Root_Hera_Model'Class
   is
   begin
      return Model : Planet_Model_Type;
   end Planet_Model;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Model     : in out Planet_Model_Type;
      User      : Hera.UI.UI_Account;
      Arguments : String)
   is
      Corporation : constant Hera.Corporations.Corporation_Type :=
        Hera.Corporations.Get (User.User_Name);
      Planet      : constant Hera.Planets.Planet_Type :=
        Corporation.Home_Planet;
   begin

      Root_Hera_Model (Model).Start (User, Arguments);
      Model.Planet := Planet;

   end Start;

end Hera.UI.Models.Planets;
