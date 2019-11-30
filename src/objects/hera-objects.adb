with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;

with Ada.Calendar;
with Hera.Real_Images;

with Hera.Logging;

with Hera.UI;

package body Hera.Objects is

   Log_Updates : constant Boolean := False;

   package Update_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Root_Update_Type'Class);

   type Hera_Object_Update is access all Root_Hera_Object'Class;

   package Object_Tables is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Hera.Identifiers.Object_Identifier,
        Element_Type    => Hera_Object_Update,
        Hash            => Hash,
        Equivalent_Keys => Hera.Identifiers."=");

   Update_List     : Update_Lists.List;
   Update_Table    : Object_Tables.Map;

   package Watcher_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Watcher_Interface'Class);

   package Watcher_Tables is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Hera.Identifiers.Object_Identifier,
        Element_Type    => Watcher_Lists.List,
        Hash            => Hash,
        Equivalent_Keys => Hera.Identifiers."=",
        "="             => Watcher_Lists."=");

   Watchers : Watcher_Tables.Map;

   ----------------
   -- Add_Update --
   ----------------

   procedure Add_Update
     (Target : Root_Hera_Object'Class;
      Update : Root_Update_Type'Class)
   is
   begin
      Add_Update (Target.Identifier, Update);
   end Add_Update;

   ----------------
   -- Add_Update --
   ----------------

   procedure Add_Update
     (Target : Hera.Identifiers.Object_Identifier;
      Update : Root_Update_Type'Class)
   is
      Added_Update : Root_Update_Type'Class := Update;
   begin
      Added_Update.Target := Target;
      Update_List.Append (Added_Update);
   end Add_Update;

   -----------------
   -- Add_Watcher --
   -----------------

   procedure Add_Watcher
     (To      : Root_Hera_Object'Class;
      Watcher : Watcher_Interface'Class)
   is
   begin
      if not Watchers.Contains (To.Identifier) then
         Watchers.Insert (To.Identifier, Watcher_Lists.Empty_List);
      end if;
      Watchers (To.Identifier).Append (Watcher);
   end Add_Watcher;

   -------------------
   -- Apply_Updates --
   -------------------

   procedure Apply_Updates is
      package Id_Sets is
        new Ada.Containers.Hashed_Sets
          (Element_Type        => Hera.Identifiers.Object_Identifier,
           Hash                => Hash,
           Equivalent_Elements => Hera.Identifiers."=",
           "="                 => Hera.Identifiers."=");
      Changed : Id_Sets.Set;
      Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Count : Natural := 0;
   begin
      while not Update_List.Is_Empty loop
         declare
            List  : constant Update_Lists.List := Update_List;
         begin
            Update_List.Clear;

            for Update of List loop
               if not Update_Table.Contains (Update.Target) then
                  raise Constraint_Error with
                    "update for unknown object with id ["
                    & String (Update.Target)
                    & "]: "
                    & Update.Description;
               end if;

               if Log_Updates then
                  Hera.Logging.Log
                    (String (Update.Target), Update.Description);
               end if;

               if Watchers.Contains (Update.Target) then
                  Changed.Include (Update.Target);
               end if;

               Update.Execute (Update_Table.Element (Update.Target).all);
               Count := Count + 1;
            end loop;
         end;
      end loop;

      for Id of Changed loop
         for Watcher of Watchers (Id) loop
            Watcher.On_Object_Changed
              (Hera_Object (Update_Table.Element (Id)));
         end loop;
      end loop;

      Hera.UI.After_Update;

      declare
         use Ada.Calendar;
         Elapsed : constant Duration := Clock - Start;
      begin
         Hera.Logging.Log
           ("update",
            "applied" & Count'Image & " updates in "
            & Hera.Real_Images.Approximate_Image (Real (Elapsed) * 1000.0)
            & "ms");
      end;
   end Apply_Updates;

   ------------
   -- Create --
   ------------

   procedure Create
     (Update : in out Root_Update_Type;
      Target : Root_Hera_Object'Class)
   is
   begin
      Update.Target := Target.Identifier;
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Object  : in out Root_Hera_Object'Class;
      Version : Object_Version)
   is
   begin
      Object.Identifier := Hera.Identifiers.Next_Identifier;
      Object.Version := Version;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Object  : in out Root_Localised_Object'Class;
      Version : Object_Version;
      Tag     : String)
   is
   begin
      Object.Initialize (Version);
      Object.Tag := Ada.Strings.Unbounded.To_Unbounded_String (Tag);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Object  : in out Root_Named_Object'Class;
      Version : Object_Version;
      Name    : String)
   is
   begin
      Object.Initialize (Version);
      Object.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Initialize;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Object : in out Root_Hera_Object;
      Config : Tropos.Configuration)
   is
   begin
      Object.Version :=
        Object_Version (String'(Config.Get ("version")));
      Object.Identifier :=
        Hera.Identifiers.Object_Identifier
          (String'(Config.Get ("identifier")));
   exception
      when others =>
         raise Object_Deserialization_Error;
   end Load;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Object : in out Root_Localised_Object;
      Config : Tropos.Configuration)
   is
   begin
      Root_Hera_Object (Object).Load (Config);
      Object.Tag :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (String'(Config.Get ("tag")));
   end Load;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Object : in out Root_Named_Object; Config : Tropos.Configuration)
   is
   begin
      Root_Hera_Object (Object).Load (Config);
      Object.Name :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (String'(Config.Get ("name")));
   end Load;

   ---------
   -- Log --
   ---------

   procedure Log
     (Object  : Root_Hera_Object'Class;
      Message : String)
   is
   begin
      Hera.Logging.Log
        (Object.Log_Id, Message);
   end Log;

   ----------
   -- Name --
   ----------

   function Name (Object : Root_Named_Object) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Object.Name);
   end Name;

   --------------------------
   -- New_Localised_Object --
   --------------------------

   function New_Localised_Object
     (Object  : Root_Localised_Object;
      Version : Object_Version;
      Tag     : String)
      return Hera_Object
   is
      Class_Object : Root_Localised_Object'Class renames
                       Root_Localised_Object'Class (Object);
      Result       : constant Hera_Object_Update :=
                       new Root_Localised_Object'Class'(Class_Object);
   begin
      Root_Localised_Object (Result.all).Initialize (Version, Tag);
      Update_Table.Insert
        (Key      => Result.Identifier,
         New_Item => Result);
      return Hera_Object (Result);
   end New_Localised_Object;

   ----------------------
   -- New_Named_Object --
   ----------------------

   function New_Named_Object
     (Object  : Root_Named_Object;
      Version : Object_Version;
      Name    : String)
      return Hera_Object
   is
      Class_Object : Root_Named_Object'Class renames
                       Root_Named_Object'Class (Object);
      Result       : constant Hera_Object_Update :=
                       new Root_Named_Object'Class'(Class_Object);
   begin
      Root_Named_Object (Result.all).Initialize (Version, Name);
      Update_Table.Insert
        (Key      => Result.Identifier,
         New_Item => Result);
      return Hera_Object (Result);
   end New_Named_Object;

   ----------------
   -- New_Object --
   ----------------

   function New_Object
     (Object  : Root_Hera_Object;
      Version : Object_Version)
      return Hera_Object
   is
      Class_Object : Root_Hera_Object'Class renames
                       Root_Hera_Object'Class (Object);
      Result       : constant Hera_Object_Update :=
                       new Root_Hera_Object'Class'(Class_Object);
   begin
      Result.Initialize (Version);
      Update_Table.Insert
        (Key      => Result.Identifier,
         New_Item => Result);
      return Hera_Object (Result);
   end New_Object;

   --------------------
   -- Remove_Watcher --
   --------------------

   procedure Remove_Watcher
     (From    : Root_Hera_Object'Class;
      Watcher : Watcher_Interface'Class)
   is
      pragma Assert (Watchers.Contains (From.Identifier));
      List : Watcher_Lists.List renames Watchers (From.Identifier);
      Position : Watcher_Lists.Cursor := List.Find (Watcher);
      pragma Assert (Watcher_Lists.Has_Element (Position));
   begin
      List.Delete (Position);
   end Remove_Watcher;

   procedure Remove_Watcher
     (Watcher : Watcher_Interface'Class)
   is
   begin
      for List of Watchers loop
         declare
            Position : Watcher_Lists.Cursor :=
              List.Find (Watcher);
         begin
            if Watcher_Lists.Has_Element (Position) then
               List.Delete (Position);
            end if;
         end;
      end loop;
   end Remove_Watcher;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Object : Root_Hera_Object;
      Config : in out Tropos.Configuration)
   is
   begin
      Config.Add ("version", String (Object.Version));
      Config.Add ("identifier", String (Object.Identifier));
   end Save;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Object : Root_Localised_Object;
      Config : in out Tropos.Configuration)
   is
   begin
      Root_Hera_Object (Object).Save (Config);
      Config.Add ("tag", Tag (Object));
   end Save;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Object : Root_Named_Object; Config : in out Tropos.Configuration)
   is
   begin
      Root_Hera_Object (Object).Save (Config);
      Config.Add ("name", Name (Object));
   end Save;

   -----------------
   -- Save_Object --
   -----------------

   function Save_Object
     (Object : Root_Hera_Object'Class)
      return Hera_Object
   is
      Result       : constant Hera_Object_Update :=
        new Root_Hera_Object'Class'(Object);
   begin
      Update_Table.Insert
        (Key      => Object.Identifier,
         New_Item => Result);
      return Hera_Object (Result);
   end Save_Object;

   ---------------
   -- Serialize --
   ---------------

   overriding procedure Serialize
     (Object    : Root_Hera_Object;
      Detail    : Hera.Serialization.Detail_Level_Type;
      Knowledge : Hera.Knowledge.Knowledge_Interface'Class;
      Json      : in out Hera.Json.Json_Object'Class)
   is
      pragma Unreferenced (Detail);
   begin
      Object.Set_Property
        (Json, Knowledge, Hera.Knowledge.Exists,
         "version", String (Object.Version));
      Object.Set_Property
        (Json, Knowledge, Hera.Knowledge.Exists,
         "identifier", String (Object.Identifier));
   end Serialize;

   ---------------
   -- Serialize --
   ---------------

   overriding procedure Serialize
     (Object    : Root_Localised_Object;
      Detail    : Hera.Serialization.Detail_Level_Type;
      Knowledge : Hera.Knowledge.Knowledge_Interface'Class;
      Json      : in out Hera.Json.Json_Object'Class)
   is
   begin
      Root_Hera_Object (Object).Serialize (Detail, Knowledge, Json);
      Object.Set_Property
        (Json, Knowledge, Hera.Knowledge.Exists,
         "tag", Tag (Object));
   end Serialize;

   ---------------
   -- Serialize --
   ---------------

   overriding procedure Serialize
     (Object    : Root_Named_Object;
      Detail    : Hera.Serialization.Detail_Level_Type;
      Knowledge : Hera.Knowledge.Knowledge_Interface'Class;
      Json      : in out Hera.Json.Json_Object'Class)
   is
   begin
      Root_Hera_Object (Object).Serialize (Detail, Knowledge, Json);
      Object.Set_Property
        (Json, Knowledge, Hera.Knowledge.Exists,
         "name", Name (Object));
   end Serialize;

   ---------
   -- Tag --
   ---------

   function Tag (Object : Root_Localised_Object) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Object.Tag);
   end Tag;

end Hera.Objects;
