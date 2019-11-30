with Ada.Containers.Vectors;

with Hera.UI.Models.Data_Source;

with Hera.Knowledge;

with Hera.Corporations;
with Hera.Planets;
with Hera.Sectors;

package body Hera.UI.Models.Planets is

   package Sector_Vectors is
     new Ada.Containers.Vectors
       (Positive, Hera.Sectors.Sector_Type,
        Hera.Sectors."=");

   type Planet_Model_Type is new Root_Hera_Model with
      record
         Planet   : Hera.Planets.Planet_Type;
         Sectors  : Sector_Vectors.Vector;
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
   begin
      Result.Set_Property
        ("title", Model.Planet.Name);

      Result.Set_Property ("width", Model.Planet.Surface_Width);
      Result.Set_Property ("height", Model.Planet.Surface_Height);

      for Item of Model.Sectors loop
         declare
            Sector : Json.Json_Object;

            procedure Set (Name : String;
                           X    : String);

            procedure Set (Name : String;
                           X    : Integer);

            procedure Set (Name : String;
                           X    : String)
            is
            begin
               Sector.Set_Property (Name, X);
            end Set;

            ---------
            -- Set --
            ---------

            procedure Set (Name : String;
                           X    : Integer)
            is
            begin
               Sector.Set_Property (Name, X);
            end Set;

         begin
            Sector.Set_Property ("id", String (Item.Identifier));
            Set ("x", Item.X);
            Set ("y", Item.Y);
            Set ("terrain", Item.Terrain.Tag);
            Sectors.Append (Sector);
         end;
      end loop;

      Result.Set_Property ("sectors", Sectors);

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

      for Sector of Planet.Get_Sectors loop
         Model.Sectors.Append (Sector);
      end loop;

   end Start;

end Hera.UI.Models.Planets;
