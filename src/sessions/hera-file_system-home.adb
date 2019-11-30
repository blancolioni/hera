with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;

with Hera.File_System.Db_FS;

with Hera.Db.Colony;
with Hera.Db.Faction;
with Hera.Db.World;

package body Hera.File_System.Home is

   function Get_Reference
     (World : Hera.Db.World.World_Type)
      return Hera.Db.World_Reference
   is (World.Get_World_Reference);

   function Get_Owned_World
     (Faction : Hera.Db.Faction_Reference;
      Name    : String)
      return Hera.Db.World_Reference;

   procedure Iterate_Owned_Worlds
     (Faction : Hera.Db.Faction_Reference;
      Process : not null access
        procedure (World : Hera.Db.World.World_Interface'Class));

   function World_Contents
     (World : Hera.Db.World.World_Type)
      return String;

   package World_Directory is
     new Hera.File_System.Db_FS
       (Container_Handle      => Hera.Db.Faction_Reference,
        Record_Reference      => Hera.Db.World_Reference,
        Get_Record            => Hera.Db.World.Get,
        Get_Reference         => Get_Reference,
        Get_Reference_By_Name => Get_Owned_World,
        Null_Record_Reference => Hera.Db.Null_World_Reference,
        Record_Interface      => Hera.Db.World.World_Interface,
        Iterate               => Iterate_Owned_Worlds,
        Contents              => World_Contents,
        "="                   => Hera.Db."=");

   type Faction_Node_Id is
     new Node_Id_Interface with
      record
         Ref : Hera.Db.Faction_Reference;
      end record;

   overriding function Is_Empty
     (Id : Faction_Node_Id)
      return Boolean;

   overriding function Get
     (Id : Faction_Node_Id)
      return Node_Interface'Class;

   overriding function Update
     (Node : Faction_Node_Id)
      return access Node_Interface'Class;

   type Faction_Node_Record is
     new Branch_Node with
      record
         Ref : Hera.Db.Faction_Reference;
      end record;

   overriding function Has_Child
     (Node : Faction_Node_Record;
      Name : String)
      return Boolean;

   overriding function Get_Child
     (Node  : Faction_Node_Record;
      Child : String)
      return Node_Id;

   overriding procedure Bind_Child
     (Node  : in out Faction_Node_Record;
      Name  : String;
      Child : Node_Id);

   overriding procedure Delete_Child
     (Node   : in out Faction_Node_Record;
      Name   : String);

   overriding procedure Iterate_Children
     (Node    : Faction_Node_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Id));

   type Home_Record is
     new Branch_Node with null record;

   overriding function Has_Child
     (Node : Home_Record;
      Name : String)
      return Boolean;

   overriding function Get_Child
     (Node  : Home_Record;
      Child : String)
      return Node_Id;

   overriding procedure Bind_Child
     (Node  : in out Home_Record;
      Name  : String;
      Child : Node_Id);

   overriding procedure Delete_Child
     (Node   : in out Home_Record;
      Name   : String);

   overriding procedure Iterate_Children
     (Node    : Home_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Id));

   ----------------
   -- Bind_Child --
   ----------------

   overriding procedure Bind_Child
     (Node  : in out Home_Record;
      Name  : String;
      Child : Node_Id)
   is
   begin
      raise Constraint_Error with
        "read-only filesystem";
   end Bind_Child;

   ----------------
   -- Bind_Child --
   ----------------

   overriding procedure Bind_Child
     (Node  : in out Faction_Node_Record;
      Name  : String;
      Child : Node_Id)
   is
   begin
      raise Constraint_Error with
        "read-only filesystem";
   end Bind_Child;

   ------------------
   -- Delete_Child --
   ------------------

   overriding procedure Delete_Child
     (Node   : in out Home_Record;
      Name   : String)
   is
   begin
      raise Constraint_Error with
        "read-only filesystem";
   end Delete_Child;

   ------------------
   -- Delete_Child --
   ------------------

   overriding procedure Delete_Child
     (Node   : in out Faction_Node_Record;
      Name   : String)
   is
   begin
      raise Constraint_Error with
        "read-only filesystem";
   end Delete_Child;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Id : Faction_Node_Id)
      return Node_Interface'Class
   is
   begin
      return Faction_Node_Record'
        (Ref => Id.Ref);
   end Get;

   ---------------
   -- Get_Child --
   ---------------

   overriding function Get_Child
     (Node  : Home_Record;
      Child : String)
      return Node_Id
   is
      pragma Unreferenced (Node);
   begin
      return Faction_Node_Id'
        (Ref =>
           Hera.Db.Faction.First_Reference_By_Name (Child));
   end Get_Child;

   ---------------
   -- Get_Child --
   ---------------

   overriding function Get_Child
     (Node  : Faction_Node_Record;
      Child : String)
      return Node_Id
   is
      pragma Unreferenced (Child);
   begin
      return World_Directory.Get_Container_Node_Id (Node.Ref);
   end Get_Child;

   ---------------------
   -- Get_Owned_World --
   ---------------------

   function Get_Owned_World
     (Faction : Hera.Db.Faction_Reference;
      Name    : String)
      return Hera.Db.World_Reference
   is
      use Hera.Db;
      World   : constant Hera.Db.World_Reference :=
                  Hera.Db.World.First_Reference_By_Name (Name);
      Colony  : constant Hera.Db.Colony.Colony_Type :=
                  Hera.Db.Colony.Get_By_World (World);
   begin
      if Colony.Has_Element
        and then Colony.Faction = Faction
      then
         return World;
      else
         return Null_World_Reference;
      end if;
   end Get_Owned_World;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Node : Home_Record;
      Name : String)
      return Boolean
   is
      pragma Unreferenced (Node);
      use type Hera.Db.Faction_Reference;
   begin
      return Hera.Db.Faction.First_Reference_By_Name (Name)
        /= Hera.Db.Null_Faction_Reference;
   end Has_Child;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Node : Faction_Node_Record;
      Name : String)
      return Boolean
   is
      pragma Unreferenced (Node);
   begin
      return Name = "worlds";
   end Has_Child;

   ---------------
   -- Home_Node --
   ---------------

   function Home_Node return Node_Interface'Class is
   begin
      return Home : Home_Record;
   end Home_Node;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty
     (Id : Faction_Node_Id)
      return Boolean
   is
      use type Hera.Db.Faction_Reference;
   begin
      return Id.Ref = Hera.Db.Null_Faction_Reference;
   end Is_Empty;

   ----------------------
   -- Iterate_Children --
   ----------------------

   overriding procedure Iterate_Children
     (Node    : Home_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Id))
   is
      pragma Unreferenced (Node);
   begin
      for Faction of Hera.Db.Faction.Scan_By_Name loop
         Process (Faction.Name,
                  Faction_Node_Id'
                    (Ref => Faction.Get_Faction_Reference));
      end loop;
   end Iterate_Children;

   ----------------------
   -- Iterate_Children --
   ----------------------

   overriding procedure Iterate_Children
     (Node    : Faction_Node_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Id))
   is
   begin
      Process ("worlds", World_Directory.Get_Container_Node_Id (Node.Ref));
   end Iterate_Children;

   --------------------------
   -- Iterate_Owned_Worlds --
   --------------------------

   procedure Iterate_Owned_Worlds
     (Faction : Hera.Db.Faction_Reference;
      Process : not null access
        procedure (World : Hera.Db.World.World_Interface'Class))
   is
   begin
      for Colony of Hera.Db.Colony.Select_By_Faction (Faction) loop
         Process (Hera.Db.World.Get (Colony.World));
      end loop;
   end Iterate_Owned_Worlds;

   ------------
   -- Update --
   ------------

   overriding function Update
     (Node : Faction_Node_Id)
      return access Node_Interface'Class
   is
      pragma Unreferenced (Node);
   begin
      return (raise Constraint_Error with
                "read-only filesystem");
   end Update;

   --------------------
   -- World_Contents --
   --------------------

   function World_Contents
     (World : Hera.Db.World.World_Type)
      return String
   is
      package String_Vectors is
        new Ada.Containers.Indefinite_Vectors (Positive, String);

      Headings : String_Vectors.Vector;
      Values   : String_Vectors.Vector;

      procedure Add (Field_Name : String);

      function Collate return String;

      ---------
      -- Add --
      ---------

      procedure Add (Field_Name : String) is
      begin
         Headings.Append (Field_Name);
         Values.Append (World.Get (Field_Name));
      end Add;

      -------------
      -- Collate --
      -------------

      function Collate return String is
         use Ada.Strings.Unbounded;
         Result : Unbounded_String;
      begin
         for I in 1 .. Headings.Last_Index loop
            Result := Result & Headings.Element (I) & ": "
              & Values.Element (I) & Character'Val (10);
         end loop;
         return To_String (Result);
      end Collate;

   begin
      Add ("name");
      Add ("category");
      Add ("climate");
      Add ("habitability");

      return Collate;
   end World_Contents;

end Hera.File_System.Home;
