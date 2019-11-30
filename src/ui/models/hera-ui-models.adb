with Ada.Containers.Vectors;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with WL.Images.Bitmaps;

with Hera.Color;

with Hera.UI.Models.Data_Source;

with Hera.Paths;

package body Hera.UI.Models is

   package Palette_Vectors is
     new Ada.Containers.Vectors (Natural, Hera.Color.Hera_Color,
                                 Hera.Color."=");

   Star_Spectrum_Palette     : Palette_Vectors.Vector;
   World_Temperature_Palette : Palette_Vectors.Vector;

   procedure Load_Spectrum_Palette with Unreferenced;
   procedure Load_World_Temperature_Palette with Unreferenced;

   -----------
   -- Close --
   -----------

   procedure Close
     (Model : in out Hera_Model)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Root_Hera_Model'Class, Hera_Model);
   begin
      Free (Model);
      Model := null;
   end Close;

   -----------
   -- Error --
   -----------

   function Error
     (Model   : Root_Hera_Model'class;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class;
      Message : String)
      return Hera.Json.Json_Value'Class
   is
      pragma Unreferenced (State);
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error,
                "error in model " & Model.Name & " client" & Client'Image);
      Put_Line ("request: " & Request.Image);
      Put_Line (Standard_Error,
                "message: " & Message);
      return Response : Hera.Json.Json_Object do
         Response.Set_Property ("error", Message);
      end return;

   end Error;

   ---------------------------
   -- Load_Spectrum_Palette --
   ---------------------------

   procedure Load_Spectrum_Palette is
      Tag               : constant String := "star_spectrum";
      Palette_File_Name : constant String :=
        Hera.Paths.Config_File
          ("star-systems/palettes/" & Tag & ".bmp");
      Reader            : WL.Images.Bitmaps.Bitmap_Image_Reader;
      Image             : WL.Images.Image_Type;
   begin
      Reader.Read (Palette_File_Name, Image);

      for X in 1 .. Image.Width loop
         declare
            Color : constant WL.Images.Image_Color :=
              Image.Color (X, 1);
         begin
            Star_Spectrum_Palette.Append
              (Hera.Color.Hera_Color'
                 (Red   => Real (Color.Red) / 255.0,
                  Green => Real (Color.Green) / 255.0,
                  Blue  => Real (Color.Blue) / 255.0,
                  Alpha => 1.0));
         end;
      end loop;
   end Load_Spectrum_Palette;

   ------------------------------------
   -- Load_World_Temperature_Palette --
   ------------------------------------

   procedure Load_World_Temperature_Palette is
      Tag               : constant String := "world-temperature";
      Palette_File_Name : constant String :=
        Hera.Paths.Config_File
          ("star-systems/palettes/" & Tag & ".bmp");
      Reader            : WL.Images.Bitmaps.Bitmap_Image_Reader;
      Image             : WL.Images.Image_Type;
   begin
      Reader.Read (Palette_File_Name, Image);

      for X in 1 .. Image.Width loop
         declare
            Color : constant WL.Images.Image_Color :=
              Image.Color (X, 1);
         begin
            World_Temperature_Palette.Append
              (Hera.Color.Hera_Color'
                 (Red   => Real (Color.Red) / 255.0,
                  Green => Real (Color.Green) / 255.0,
                  Blue  => Real (Color.Blue) / 255.0,
                  Alpha => 1.0));
         end;
      end loop;
   end Load_World_Temperature_Palette;

   ---------------
   -- Serialize --
   ---------------

--     function Serialize
--       (Object     : Hera.Db.Orbiting_Object.Orbiting_Object_Type;
--        Parameters : Json.Json_Value'Class)
--        return Json.Json_Value'Class
--     is
--
--        use Hera.Calendar;
--        use Hera.Db;
--
--        Result : Json.Json_Object;
--
--        Name         : constant String := Parameters.Get_Property ("name");
--        Detail_Image : constant String :=
--          Parameters.Get_Property ("detail");
--
--        Detail       : constant Detail_Level :=
--          (if Name /= "null" and then Name /= Object.Name
--           then Low
--           elsif Detail_Image = ""
--           or else Detail_Image = "null"
--           or else Detail_Image = "1"
--           or else Detail_Image = "medium"
--           then Medium
--           elsif Detail_Image = "0"
--           or else Detail_Image = "low"
--           then Low
--           elsif Detail_Image = "2"
--           or else Detail_Image = "high"
--           then High
--           else Medium);
--
--        procedure Set (Property_Name, Property_Value : String);
--        procedure Set (Property_Name  : String;
--                       Property_Value : Real);
--        procedure Set (Property_Name  : String;
--                       Property_Value : Json.Json_Value'Class);
--
--        function Orbiting_Objects
--          (Primary : Hera.Db.Star_System_Object_Reference)
--           return Json.Json_Array;
--
--        ----------------------
--        -- Orbiting_Objects --
--        ----------------------
--
--        function Orbiting_Objects
--          (Primary : Hera.Db.Star_System_Object_Reference)
--           return Json.Json_Array
--        is
--           Primary_Mass : constant Hera.Db.Massive_Object_Reference :=
--             Hera.Db.Star_System_Object.Get (Primary)
--             .Get_Massive_Object_Reference;
--        begin
--           return Arr : Json.Json_Array do
--              for Item of
--                Hera.Db.Orbiting_Object.Select_By_Primary_Massive
--                  (Primary_Mass)
--              loop
--                 Arr.Append (Serialize (Item, Parameters));
--              end loop;
--           end return;
--        end Orbiting_Objects;
--
--        ---------
--        -- Set --
--        ---------
--
--        procedure Set (Property_Name, Property_Value : String) is
--        begin
--           Result.Set_Property (Property_Name, Property_Value);
--        end Set;
--
--        ---------
--        -- Set --
--        ---------
--
--        procedure Set (Property_Name  : String;
--                       Property_Value : Real)
--        is
--        begin
--           Result.Set_Property (Property_Name, Float (Property_Value));
--        end Set;
--
--        ---------
--        -- Set --
--        ---------
--
--        procedure Set (Property_Name  : String;
--                       Property_Value : Json.Json_Value'Class)
--        is
--        begin
--           Result.Set_Property (Property_Name, Property_Value);
--        end Set;
--
--     begin
--
--        if Star_Spectrum_Palette.Is_Empty then
--           Load_Spectrum_Palette;
--        end if;
--
--        if World_Temperature_Palette.Is_Empty then
--           Load_World_Temperature_Palette;
--        end if;
--
--        Set ("title", Object.Name);
--        Set ("name", Object.Name);
--        Set ("id", Object.Identifier);
--        Set ("orbit",
--             Object.Semimajor_Axis);
--        Set ("longitude",
--             (if Object.Primary_Massive = Null_Massive_Object_Reference
--              then 0.0
--              else Hera.Orbits.Calculate_Longitude
--                (Massive_Object.Get (Object.Primary_Massive).Mass,
--                 Object.Semimajor_Axis,
--                 Hera.Calendar.Clock - Object.Epoch))
--             * Ada.Numerics.Pi / 180.0);
--        declare
--           use Hera.Db.Star_System_Object;
--           SSO : constant Star_System_Object_Type :=
--             Get_Star_System_Object
--               (Object.Get_Orbiting_Object_Reference);
--           Deps : Json.Json_Array;
--        begin
--           if SSO.Has_Element then
--          Deps := Orbiting_Objects (SSO.Get_Star_System_Object_Reference);
--           end if;
--           Set ("dependents", Deps);
--        end;
--
--        if Detail > Low then
--           Set ("year", Object.Period);
--
--           if Object.Top_Record in R_Star | R_World then
--              Set ("axisTilt",
--                   Hera.Db.Star_System_Object.Get_Star_System_Object
--                     (Object.Get_Orbiting_Object_Reference)
--                   .Tilt);
--           end if;
--        end if;
--
--        if Object.Top_Record = R_Star then
--           declare
--              Star  : constant Hera.Stars.Star_Type'Class :=
--                Hera.Stars.Get
--                  (Hera.Db.Star.Get_Star
--                     (Object.Get_Orbiting_Object_Reference));
--              Color : constant Hera.Color.Hera_Color :=
--                Star_Spectrum_Palette.Element
--                  (Natural'Min
--                     (Natural (Real'Max (Star.Temperature - 800.0, 0.0)
--                      / 29200.0),
--                      Star_Spectrum_Palette.Last_Index));
--           begin
--              Set ("type", "STAR");
--              if Detail > Low then
--                 Set ("mass", Object.Mass);
--                 Set ("radius",
--                      Star.Radius);
--                 Set ("temperature", Star.Temperature);
--                 Set ("red", Color.Red);
--                 Set ("green", Color.Green);
--                 Set ("blue", Color.Blue);
--              end if;
--           end;
--        elsif Object.Top_Record = R_World then
--           declare
--              World  : constant Hera.Db.World.World_Type :=
--                Hera.Db.World.Get_World
--                  (Object.Get_Orbiting_Object_Reference);
--           begin
--              Set ("type", "WORLD");
--              if Detail > Low then
--                 Set ("mass", Object.Mass);
--                 Set ("radius",
--                      World.Radius);
--                 Set ("temperature", World.Average_Temperature);
--                 Set ("composition",
--                      Hera.Db.World_Composition'Image
--                        (World.Composition));
--                 Set ("climate",
--                      Hera.Db.World_Climate'Image
--                        (World.Climate));
--                 Set ("day", World.Rotation_Period);
--
--                 if World.Gas_Giant or else Detail < High then
--                    declare
--                       Sectors : Json.Json_Array;
--                    begin
--                       Set ("surface", Sectors);
--                    end;
--                 else
--                    declare
--                       Sectors : Json.Json_Array;
--
--                       function Serialize
--                         (Vertex : Hera.Worlds.Sector_Vertex)
--                       return Json.Json_Object;
--
--                       procedure Add_Sector
--                         (Sector : Hera.Db.World_Sector_Reference);
--
--                       function Sector_Color
--                         (Sector : Hera.Db.World_Sector_Reference)
--                          return Hera.Color.Hera_Color;
--
--                       ----------------
--                       -- Add_Sector --
--                       ----------------
--
--                       procedure Add_Sector
--                         (Sector : Hera.Db.World_Sector_Reference)
--                       is
--                          use Hera.Worlds;
--                          Vertices : constant Sector_Vertex_Array :=
--                                       Get_Vertices (Sector);
--                          Centre   : constant Sector_Vertex :=
--                            Get_Centre (Sector);
--                          Color     : constant Hera.Color.Hera_Color :=
--                            Sector_Color (Sector);
--                          Arr      : Json.Json_Array;
--                          Obj      : Json.Json_Object;
--                       begin
--                          for Vertex of Vertices loop
--                             Arr.Append (Serialize (Vertex));
--                          end loop;
--                          Obj.Set_Property ("border", Arr);
--                          Obj.Set_Property ("normal", Serialize (Centre));
--                          Obj.Set_Property ("color",
--                                            Hera.Color.To_Html_String
--                                              (Color));
--                          Obj.Set_Property
--                            ("model",
--                             (if Is_Urban (Sector) then "city" else ""));
--                          Sectors.Append (Obj);
--                       end Add_Sector;
--
--                       ------------------
--                       -- Sector_Color --
--                       ------------------
--
--                       function Sector_Color
--                         (Sector : Hera.Db.World_Sector_Reference)
--                          return Hera.Color.Hera_Color
--                       is
--                          Temperature_Model : constant Boolean := False;
--                          Faction : constant Faction_Reference :=
--                            Hera.Worlds.Get_Owner (Sector);
--                          WS : constant Hera.Db.World_Sector
--                            .World_Sector_Type :=
--                              Hera.Db.World_Sector.Get (Sector);
--                       begin
--                          if Hera.Worlds.Is_Urban (Sector) then
--                             return Hera.Color.From_String ("#0000FF");
--                          elsif Faction /= Null_Faction_Reference then
--                             return Hera.Factions.Get (Faction).Color;
--                          elsif Temperature_Model then
--                             declare
--                                K : constant Real :=
--                                  Hera.Worlds.Get_Average_Temperature
--                                    (Sector);
--                                I : constant Natural :=
--                                  Integer'Max
--                                    (0,
--                                     Integer'Min
--                                       (World_Temperature_Palette.Last_Index,
--                                        Integer ((K - 247.4) * 20.0)));
--                             begin
--                                return World_Temperature_Palette.Element (I);
--                             end;
--                          elsif WS.Feature /= Null_Feature_Reference then
--                             declare
--                                use Hera.Db.Feature;
--                                F : constant Feature_Type :=
--                                  Get (WS.Feature);
--                             begin
--                                return (F.Red, F.Green, F.Blue, 1.0);
--                             end;
--                          else
--                             declare
--                                E : constant Elevation.Elevation_Type :=
--                                  Hera.Db.Elevation.Get
--                                    (Hera.Worlds.Get_Elevation (Sector));
--                             begin
--                                return (E.Red, E.Green, E.Blue, 1.0);
--                             end;
--                          end if;
--                       end Sector_Color;
--
--                       ---------------
--                       -- Serialize --
--                       ---------------
--
--                       function Serialize
--                         (Vertex : Hera.Worlds.Sector_Vertex)
--                          return Json.Json_Object
--                       is
--                       begin
--                          return Obj : Json.Json_Object do
--                             Obj.Set_Property ("x", Float (Vertex.X));
--                             Obj.Set_Property ("y", Float (Vertex.Y));
--                             Obj.Set_Property ("z", Float (Vertex.Z));
--                          end return;
--                       end Serialize;
--
--                    begin
--                       Hera.Worlds.Scan_Surface
--                         (World.Get_World_Reference,
--                          Add_Sector'Access);
--                       Set ("surface", Sectors);
--                    end;
--                 end if;
--              end if;
--           end;
--
--        elsif Object.Top_Record = R_Ship then
--           declare
--              Ship : constant Hera.Db.Ship.Ship_Type :=
--                Hera.Db.Ship.Get_Ship (Object.Get_Orbiting_Object_Reference);
--           begin
--              Set ("type", "SHIP");
--              Set ("radius", 100.0);
--              Set ("mass", Ship.Mass);
--           end;
--        end if;
--
--        return Result;
--
--     exception
--        when E : others =>
--           raise Constraint_Error with
--             "exception while serializing "
--             & Record_Type'Image (Object.Top_Record)
--             & " "
--             & Object.Name
--             & ": "
--             & Ada.Exceptions.Exception_Message (E);
--
--     end Serialize;

   -----------
   -- Start --
   -----------

   procedure Start
     (Model     : in out Root_Hera_Model;
      User      : Hera.UI.UI_Account;
      Arguments : String)
   is
      pragma Unreferenced (Arguments);
   begin
      Model.Account := User;
   end Start;

end Hera.UI.Models;
