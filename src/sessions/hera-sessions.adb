with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Tropos.Reader;

with Hera.Authorization;
with Hera.Calendar;
with Hera.Commands.Writers;
with Hera.Money;
with Hera.Scenarios;

with Hera.File_System.Root;

with Hera.Sessions.Status;
with Hera.Outliner;

with Hera.UI.Models.Loader;

with Hera.Colonies;
with Hera.Star_Systems;

package body Hera.Sessions is

   type Status_Get_Function is access
     function (Session : Root_Hera_Session'Class)
               return Json.Json_Value'Class;

   type Status_Set_Procedure is access
     procedure (Session : in out Root_Hera_Session'Class;
                Value   : Json.Json_Value'Class);

   type Status_Setting_Record is
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
         Get  : Status_Get_Function;
         Set  : Status_Set_Procedure;
      end record;

   package Status_Setting_Maps is
     new WL.String_Maps (Status_Setting_Record);

   Status_Settings : Status_Setting_Maps.Map;

   procedure Check_Status;

   procedure Add_Status
     (Name : String;
      Get  : Status_Get_Function;
      Set  : Status_Set_Procedure);

   procedure On_Clock_Tick
     (Object : Hera.Signals.Signaler'Class;
      Data   : Hera.Signals.Signal_Data_Interface'Class);

   function Default_Dashboard
     (New_Client : not null access
        function (Model_Name : String;
                  Model_Args : String)
           return Hera.UI.Client_Id)
      return Hera.Json.Json_Value'Class;

   function Initial_Outline
     (User : Hera.UI.UI_Account)
     return Hera.Json.Json_Value'Class;

   function Return_Error (Message : String) return Json.Json_Value'Class;

   ------------------
   -- Session_Data --
   ------------------

   protected body Session_Data is

      ------------------
      -- Close_Client --
      ------------------

      procedure Close_Client
        (Client_Id      : Hera.UI.Client_Id)
      is
      begin
         Client_Map.Delete (Client_Id);
      end Close_Client;

      -------------------
      -- Create_Client --
      -------------------

      procedure Create_Client
        (User           : Hera.UI.UI_Account;
         Context        : Hera.Contexts.Context_Type;
         Model_Name     : String;
         Model_Argument : String;
         Client_Id      : out Hera.UI.Client_Id)
      is
         use type Hera.UI.Client_Id;
      begin
         Client_Id := 0;

         if not Hera.UI.Models.Loader.Exists (Model_Name) then
            return;
         end if;

         Last_Client := Last_Client + 1;

         declare
            Model : constant Hera.UI.Models.Hera_Model :=
              Hera.UI.Models.Loader.Get (Model_Name);
         begin
            Model.Start (User, Model_Argument);
            Client_Map.Insert
              (Last_Client,
               Client_Type'
                 (Model   => Model,
                  Context => Context));
         end;

         Client_Id := Last_Client;
      end Create_Client;

      ---------------------
      -- Execute_Command --
      ---------------------

      procedure Execute_Command
        (Client_Id : Hera.UI.Client_Id;
         Writer    : in out Hera.Writers.Writer_Interface'Class;
         Command   : String)
      is
      begin
         Hera.Commands.Execute_Command_Line
           (Line    => Command,
            Context => Client_Map (Client_Id).Context,
            Writer  => Writer);
      exception
         when E : others =>
            Writer.Put_Error
              (Ada.Exceptions.Exception_Message (E));
      end Execute_Command;

      ---------------------------
      -- Get_Environment_Value --
      ---------------------------

      function Get_Environment_Value
        (Name : String)
         return Json.Json_Value'Class
      is
      begin
         if Environment.Contains (Name) then
            return Environment.Element (Name);
         else
            return Hera.Json.Null_Value;
         end if;
      end Get_Environment_Value;

      ---------------
      -- Get_Model --
      ---------------

      function Get_Model
        (Client_Id : Hera.UI.Client_Id)
         return Hera.UI.Models.Hera_Model
      is
      begin
         return Client_Map.Element (Client_Id).Model;
      end Get_Model;

      --------------------
      -- Object_Changed --
      --------------------

      procedure Object_Changed
        (Object : Hera.Objects.Hera_Object)
      is
      begin
         Changed_Objects.Append (Object);
      end Object_Changed;

      ---------------
      -- Reference --
      ---------------

      procedure Reference is
      begin
         References := References + 1;
      end Reference;

      --------------------------
      -- Scan_Changed_Objects --
      --------------------------

      procedure Scan_Changed_Objects
        (Process : not null access
           procedure (Object : Hera.Objects.Hera_Object))
      is
      begin
         for Object of Changed_Objects loop
            Process (Object);
         end loop;
         Changed_Objects.Clear;
      end Scan_Changed_Objects;

      ------------------
      -- Scan_Clients --
      ------------------

      procedure Scan_Clients
        (Process : not null access
           procedure
             (Client : Hera.UI.Client_Id;
              Model  : in out Hera.UI.Models.Root_Hera_Model'Class))
      is
      begin
         for Position in Client_Map.Iterate loop
            Process (Client_Maps.Key (Position),
                     Client_Map.Reference (Position).Model.all);
         end loop;
      end Scan_Clients;

      ---------------------------
      -- Set_Environment_Value --
      ---------------------------

      procedure Set_Environment_Value
        (Name  : String;
         Value : Json.Json_Value'Class)
      is
      begin
         if Environment.Contains (Name) then
            Environment.Replace (Name, Value);
         else
            Environment.Insert (Name, Value);
         end if;
      end Set_Environment_Value;

      ---------------
      -- Set_Model --
      ---------------

      procedure Set_Model
        (Client_Id : Hera.UI.Client_Id;
         Model     : Hera.UI.Models.Hera_Model)
      is
      begin
         Client_Map (Client_Id).Model := Model;
      end Set_Model;

      -----------------
      -- Unreference --
      -----------------

      procedure Unreference (Finished : out Boolean) is
      begin
         References := References - 1;
         Finished := References = 0;

         if Finished then
            for Client of Client_Map loop
               Hera.UI.Models.Close (Client.Model);
            end loop;
         end if;

      end Unreference;

   end Session_Data;

   -----------------
   -- Add_Handler --
   -----------------

   overriding function Add_Handler
     (Session : in out Root_Hera_Session;
      Signal  : Hera.Signals.Signal_Type;
      Handler : Hera.Signals.Handler_Type;
      Data    : Hera.Signals.Signal_Data_Interface'Class)
      return Hera.Signals.Handler_Id
   is
   begin
      return Session.Dispatcher.Add_Handler (Signal, Handler, Data);
   end Add_Handler;

   ----------------
   -- Add_Status --
   ----------------

   procedure Add_Status
     (Name : String;
      Get  : Status_Get_Function;
      Set  : Status_Set_Procedure)
   is
   begin
      Status_Settings.Insert
        (Name,
         Status_Setting_Record'
           (Name => Ada.Strings.Unbounded.To_Unbounded_String (Name),
            Get  => Get,
            Set  => Set));
   end Add_Status;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Session : in out Root_Hera_Session) is
   begin
      if Session.Data /= null then
         Session.Data.Reference;
      end if;
   end Adjust;

   ------------------
   -- After_Update --
   ------------------

   overriding procedure After_Update
     (Session : Root_Hera_Session)
   is
      use Hera.Objects;

      Updated : Boolean := False;
      Message : Json.Json_Object;

      Main_Account : constant Hera_Object :=
        Hera_Object (Session.Corporation.Account);

      procedure Process (Object : Hera.Objects.Hera_Object);

      -------------
      -- Process --
      -------------

      procedure Process (Object : Hera.Objects.Hera_Object) is
      begin
         if Object = Main_Account then
            Message.Set_Property
              ("cash",
               Hera.Money.Show
                 (Session.Corporation.Account.Current_Cash));
         end if;
         Updated := True;
      end Process;

   begin
      Session.Data.Scan_Changed_Objects (Process'Access);
      if Updated then
         Message.Set_Property ("type", "update-faction");
         Session.Send_Message (Message);
      end if;
   end After_Update;

   ------------------
   -- Check_Status --
   ------------------

   procedure Check_Status is
   begin
      if Status_Settings.Is_Empty then
         Add_Status ("updateSpeed",
                     Hera.Sessions.Status.Get_Update_Speed'Access,
                     Hera.Sessions.Status.Set_Update_Speed'Access);
      end if;
   end Check_Status;

   ------------------
   -- Close_Client --
   ------------------

   overriding procedure Close_Client
     (Session   : in out Root_Hera_Session;
      Client    : Hera.UI.Client_Id)
   is
   begin
      Session.Data.Close_Client (Client);
   end Close_Client;

   -----------------------
   -- Default_Dashboard --
   -----------------------

   function Default_Dashboard
     (New_Client : not null access
        function (Model_Name : String;
                  Model_Args : String)
      return Hera.UI.Client_Id)
      return Hera.Json.Json_Value'Class
   is

      Boxes   : Hera.Json.Json_Array;
      Clients : Hera.Json.Json_Array;

      procedure Add_Box
        (Id : Natural;
         Left, Top : Positive;
         Right, Bottom : Positive;
         Child_1       : Natural := 0;
         Child_2       : Natural := 0;
         Client_Id     : Integer := -1);

      function Add_Client
        (Title      : String;
         Model_Name : String;
         Model_Args : String;
         View_Name  : String)
         return Hera.UI.Client_Id;

      -------------
      -- Add_Box --
      -------------

      procedure Add_Box
        (Id            : Natural;
         Left, Top     : Positive;
         Right, Bottom : Positive;
         Child_1       : Natural := 0;
         Child_2       : Natural := 0;
         Client_Id     : Integer := -1)
      is
         Box    : Hera.Json.Json_Object;
      begin

         Box.Set_Property ("id", Id);

         declare
            Anchor : Hera.Json.Json_Object;
         begin
            Anchor.Set_Property ("left", Left);
            Anchor.Set_Property ("top", Top);
            Anchor.Set_Property ("right", Right);
            Anchor.Set_Property ("bottom", Bottom);
            Box.Set_Property ("anchor", Anchor);
            Box.Set_Property ("clientId", Client_Id);
         end;

         if Child_1 > 0 then
            declare
               Child_Boxes : Hera.Json.Json_Array;
            begin
               Child_Boxes.Append (Json.Integer_Value (Child_1));
               Child_Boxes.Append (Json.Integer_Value (Child_2));
               Box.Set_Property ("children", Child_Boxes);
            end;
         end if;

         Boxes.Append (Box);
      end Add_Box;

      ----------------
      -- Add_Client --
      ----------------

      function Add_Client
        (Title      : String;
         Model_Name : String;
         Model_Args : String;
         View_Name  : String)
         return Hera.UI.Client_Id
      is
         Client : Json.Json_Object;
         Id     : constant Hera.UI.Client_Id :=
           New_Client (Model_Name, Model_Args);
      begin
         Client.Set_Property ("clientId", Natural (Id));
         Client.Set_Property ("viewName", View_Name);
         Client.Set_Property ("modelName", Model_Name);
         Client.Set_Property ("modelArgs", Model_Args);
         Client.Set_Property ("title", Title);
         Clients.Append (Client);
         return Id;
      end Add_Client;

      Next_Id      : Natural := 0;

   begin
      for Config of
        Tropos.Reader.Read_Config
          (Hera.Scenarios.Current_Scenario.File_Path
             ("dashboard.config"))
      loop
         declare
            function Anchor (Name : String) return Positive
            is (Config.Child ("anchor").Get (Name));

            function Child (Index : Positive) return Natural
            is (if Config.Contains ("childBoxes")
                then Config.Child ("childBoxes").Get (Index)
                else 0);

            Client_Id : Integer := -1;

         begin

            if Config.Contains ("client")
              or else Child (1) = 0
            then
               declare
                  Client : constant Tropos.Configuration :=
                    Config.Child ("client");
                  Model  : constant String :=
                    Client.Get ("model", "shell");
                  View   : constant String :=
                    Client.Get ("view",
                                Hera.UI.Models.Loader.Get (Model)
                                .Default_View_Name);
                  This_Id : constant Hera.UI.Client_Id :=
                    Add_Client
                      (Title      => Client.Get ("title", Model),
                       Model_Name => Model,
                       Model_Args => Client.Get ("arguments", ""),
                       View_Name  => View);
               begin
                  Client_Id := Natural (This_Id);
               end;
            end if;

            Add_Box
              (Id        => Next_Id,
               Left      => Anchor ("left"),
               Top       => Anchor ("top"),
               Right     => Anchor ("right"),
               Bottom    => Anchor ("bottom"),
               Child_1   => Child (1),
               Child_2   => Child (2),
               Client_Id => Client_Id);
         end;
         Next_Id := Next_Id + 1;
      end loop;

      return Dashboard : Hera.Json.Json_Object do
         Dashboard.Set_Property ("clients", Clients);
         Dashboard.Set_Property ("boxes", Boxes);
      end return;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           ("Unable to load default dashboard: "
            & Ada.Exceptions.Exception_Message (E));
         raise;
   end Default_Dashboard;

   -----------------------
   -- Environment_Value --
   -----------------------

   overriding function Environment_Value
     (Session : Root_Hera_Session;
      Name    : String)
      return Hera.Json.Json_Value'Class
   is
   begin
      return Session.Data.Get_Environment_Value (Name);
   end Environment_Value;

   ---------------------
   -- Execute_Command --
   ---------------------

   overriding function Execute_Command
     (Session : in out Root_Hera_Session;
      Client  : Hera.UI.Client_Id;
      Command : String)
      return Hera.Json.Json_Value'Class
   is
      Writer : Hera.Commands.Writers.Json_Writer;

   begin

      Session.Data.Execute_Command
        (Client, Writer, Command);
      return Writer.To_Json;

   end Execute_Command;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Session : in out Root_Hera_Session) is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Session_Data, Session_Data_Access);
   begin
      if Session.Data /= null then
         declare
            Finished : Boolean;
         begin
            Session.Data.Unreference (Finished);
            if Finished then
               for Watcher of Session.Watchers loop
                  Hera.Objects.Remove_Watcher (Watcher);
               end loop;
               Free (Session.Data);
            end if;
         end;
      end if;
   end Finalize;

   -----------------------
   -- Handle_Client_Get --
   -----------------------

   overriding function Handle_Client_Get
     (Session : Root_Hera_Session;
      Client  : Hera.UI.Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class
   is
   begin
      declare
         Model : constant Hera.UI.Models.Hera_Model :=
           Session.Data.Get_Model (Client);
      begin
         return Model.Get (Session, Client, Request);
      end;
   exception
      when E : others =>
         return Return_Error (Ada.Exceptions.Exception_Message (E));
   end Handle_Client_Get;

   ------------------------
   -- Handle_Client_Post --
   ------------------------

   overriding function Handle_Client_Post
     (Session : in out Root_Hera_Session;
      Client  : Hera.UI.Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class
   is
   begin
      declare
         Model : constant Hera.UI.Models.Hera_Model :=
           Session.Data.Get_Model (Client);
      begin
         return Model.Handle (Session, Client, Request);
      end;
   exception
      when E : others =>
         return Return_Error (Ada.Exceptions.Exception_Message (E));
   end Handle_Client_Post;

   --------------------
   -- Handle_Message --
   --------------------

   overriding function Handle_Message
     (Session    : in out Root_Hera_Session;
      Message    : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class
   is
      pragma Unreferenced (Message);
   begin
      return Session.Status_Message;
   end Handle_Message;

   ---------------------
   -- Initial_Outline --
   ---------------------

   function Initial_Outline
     (User : Hera.UI.UI_Account)
      return Hera.Json.Json_Value'Class
   is
      Outliner : Hera.Outliner.Hera_Outliner;
      Corporation : constant Hera.Corporations.Corporation_Type :=
        Hera.Corporations.Get (User.User_Name);
      Colonies    : constant Hera.Corporations.Corporate_Colony_Array :=
        Corporation.Colonies;
   begin
      for Colony of Colonies loop
         Outliner.Append
           (Hera.Outliner.New_Item
              (String (Colony.Identifier), Colony.Name));
      end loop;
      return Outliner.Serialize;
   end Initial_Outline;

   overriding procedure Initialize (Session : in out Root_Hera_Session)
   is null;

   ----------------------
   -- Is_Administrator --
   ----------------------

   overriding function Is_Administrator
     (Session   : Root_Hera_Session)
      return Boolean
   is
   begin
      return Session.User.Is_Administrator;
   end Is_Administrator;

   -------------------------------
   -- New_Administrator_Session --
   -------------------------------

   function New_Administrator_Session
     return Hera.UI.State_Interface'Class
   is
   begin
      return New_Session ("", "");
   end New_Administrator_Session;

   ----------------
   -- New_Client --
   ----------------

   overriding function New_Client
     (Session        : in out Root_Hera_Session;
      Model_Name     : String;
      Model_Argument : String)
      return Hera.UI.Client_Id
   is
   begin
      return Id : Hera.UI.Client_Id do
         Session.Data.Create_Client
           (Session.User, Session.Default_Context,
            Model_Name, Model_Argument,
            Id);
      end return;
   end New_Client;

   -----------------
   -- New_Session --
   -----------------

   function New_Session
     (User_Name : String;
      Password  : String)
      return Hera.UI.State_Interface'Class
   is
      use type Hera.UI.UI_Account;
      User : constant Hera.UI.UI_Account :=
        Hera.Authorization.Login (User_Name, Password);
      Session : Root_Hera_Session;
   begin
      if User /= null then
         Session.User := User;

         declare
            Home    : constant String := "/home/" & User.User_Name;
         begin
            if Hera.Corporations.Exists (User.User_Name) then
               Session.Default_Context.Create_Context
                 (Root          =>
                    Hera.File_System.Root.System_Root_Node_Id,
                  User          => User,
                  Default_Scope => Home);
               Session.Data := new Session_Data;
               Session.Data.Set_Environment_Value
                 ("HOME", Hera.Json.String_Value (Home));
               Session.Corporation :=
                 Hera.Corporations.Get (User.User_Name);

               declare
                  function New_Client
                    (Model_Name, Model_Args : String)
                     return Hera.UI.Client_Id
                  is (Session.New_Client (Model_Name, Model_Args));
               begin
                  Session.Data.Set_Environment_Value
                    ("DASHBOARD", Default_Dashboard (New_Client'Access));
               end;

               Session.Data.Set_Environment_Value
                 ("OUTLINE", Initial_Outline (User));

               declare
                  Watcher : constant Session_Watcher :=
                    Session_Watcher'
                      (Data => Session.Data);
               begin
                  Session.Corporation.Account.Add_Watcher (Watcher);
                  Session.Watchers.Append (Watcher);
               end;

               Session.On_Clock_Tick_Id :=
                 Session.Add_Handler
                   (Signal     => Hera.UI.Signal_Clock_Tick,
                    Handler    => On_Clock_Tick'Access,
                    Data       =>
                      Hera.Signals.Null_Signal_Data'(null record));
            else
               Session.User := null;
            end if;
         end;
      end if;
      return Session;
   end New_Session;

   -------------------
   -- On_Clock_Tick --
   -------------------

   procedure On_Clock_Tick
     (Object : Hera.Signals.Signaler'Class;
      Data   : Hera.Signals.Signal_Data_Interface'Class)
   is
      pragma Unreferenced (Data);
      Session    : Root_Hera_Session'Class renames
        Root_Hera_Session'Class (Object);
   begin
      Ada.Text_IO.Put_Line
        ("on-clock-tick: "
         & Session.User_Name);
      Session.Connection.Element.Send_Message
        (Session.Status_Message);
   end On_Clock_Tick;

   -----------------------
   -- On_Object_Changed --
   -----------------------

   overriding procedure On_Object_Changed
     (Watcher : Session_Watcher;
      Object  : Hera.Objects.Hera_Object)
   is
   begin
      Watcher.Data.Object_Changed (Object);
   end On_Object_Changed;

   --------------------
   -- Remove_Handler --
   --------------------

   overriding procedure Remove_Handler
     (Session : in out Root_Hera_Session;
      Signal  : Hera.Signals.Signal_Type;
      Id      : Hera.Signals.Handler_Id)
   is
   begin
      Session.Dispatcher.Remove_Handler (Signal, Id);
   end Remove_Handler;

   -------------------
   -- Replace_Model --
   -------------------

   overriding procedure Replace_Model
     (Session        : in out Root_Hera_Session;
      Client         : Hera.UI.Client_Id;
      Model_Name     : String;
      Model_Argument : String)
   is
      Model : constant Hera.UI.Models.Hera_Model :=
        Hera.UI.Models.Loader.Get (Model_Name);
   begin
      Model.Start (Session.User, Model_Argument);
      Session.Data.Set_Model (Client, Model);
   end Replace_Model;

   ------------------
   -- Return_Error --
   ------------------

   function Return_Error (Message : String) return Json.Json_Value'Class is
      Stdout, Stderr : Json.Json_Array;
      Resp           : Json.Json_Object;
   begin
      Stderr.Append
        (Json.String_Value (Message));
      Resp.Set_Property ("standardOutput", Stdout);
      Resp.Set_Property ("standardError", Stderr);
      return Resp;
   end Return_Error;

   ------------------
   -- Send_Message --
   ------------------

   overriding procedure Send_Message
     (Session : Root_Hera_Session;
      Message : Hera.Json.Json_Value'Class)
   is
      Msg : Json.Json_Object;
   begin

      Msg.Set_Property ("payload", Message);

      declare
         Clients : Json.Json_Array;

         Request : constant Json.Json_Value'Class :=
           Json.Deserialize ("""changes""");

         procedure Check_Client
           (Client_Id : Hera.UI.Client_Id;
            Model     : in out Hera.UI.Models.Root_Hera_Model'Class);

         ------------------
         -- Check_Client --
         ------------------

         procedure Check_Client
           (Client_Id : Hera.UI.Client_Id;
            Model     : in out Hera.UI.Models.Root_Hera_Model'Class)
         is
         begin
            if Model.Changed then
               Model.Update;
               declare
                  Element : Json.Json_Object;
               begin
                  Element.Set_Property
                    ("clientId", Natural (Client_Id));
                  Element.Set_Property
                    ("update",
                     Model.Get
                       (State   => Session,
                        Client  => Client_Id,
                        Request => Request));
                  Clients.Append (Element);
               end;
            end if;
         end Check_Client;

      begin
         Session.Data.Scan_Clients (Check_Client'Access);
         Msg.Set_Property ("clients", Clients);
      end;

      Session.Connection.Element.Send_Message (Msg);
   end Send_Message;

   -----------------
   -- Send_Signal --
   -----------------

   overriding procedure Send_Signal
     (Session : in out Root_Hera_Session;
      Signal  : Hera.Signals.Signal_Type)
   is
   begin
      Session.Dispatcher.Call_Handlers (Session, Signal);
   end Send_Signal;

   --------------------
   -- Set_Connection --
   --------------------

   overriding procedure Set_Connection
     (Session    : in out Root_Hera_Session;
      Connection : Hera.UI.Connection_Interface'Class)
   is
   begin
      Session.Connection := Connection_Holders.To_Holder (Connection);
   end Set_Connection;

   ----------------------
   -- Set_Status_Value --
   ----------------------

   overriding procedure Set_Status_Value
     (Session : in out Root_Hera_Session;
      Name    : String;
      Value   : Hera.Json.Json_Value'Class)
   is
   begin
      Check_Status;
      if Status_Settings.Contains (Name) then
         Status_Settings.Element (Name).Set (Session, Value);
      else
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "no such status: " & Name);
      end if;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "error setting " & Name & ": "
            & Ada.Exceptions.Exception_Message (E));

   end Set_Status_Value;

   --------------------
   -- Status_Message --
   --------------------

   function Status_Message
     (Session : Root_Hera_Session'Class)
      return Json.Json_Object
   is
      use type Hera.Calendar.Time;
      Now : constant Hera.Calendar.Time := Hera.Calendar.Clock;
   begin
      return Message : Json.Json_Object do
         Message.Set_Property ("type", "update-faction");
         Message.Set_Property
           ("cash",
            Float
              (Hera.Money.To_Real (Session.Corporation.Cash)));
         Message.Set_Property
           ("currentTime",
            Float (Now - Hera.Calendar.Start));
         Message.Set_Property
           ("currentTimeImage",
            Hera.Calendar.Image (Now, False));

      end return;
   end Status_Message;

   ------------------
   -- Status_Value --
   ------------------

   overriding function Status_Value
     (Session : Root_Hera_Session;
      Name    : String)
      return Hera.Json.Json_Value'Class
   is
   begin
      Check_Status;
      if Status_Settings.Contains (Name) then
         return Status_Settings.Element (Name).Get (Session);
      else
         return Result : Json.Json_Object do
            Result.Set_Property ("noSuchSetting", Name);
         end return;
      end if;
   end Status_Value;

   ---------------
   -- User_Name --
   ---------------

   overriding function User_Name
     (Session   : Root_Hera_Session)
      return String
   is
   begin
      return Session.User.User_Name;
   end User_Name;

   -----------
   -- Valid --
   -----------

   overriding function Valid
     (Session   : Root_Hera_Session)
      return Boolean
   is
      use type Hera.UI.UI_Account;
   begin
      return Session.User /= null;
   end Valid;

end Hera.Sessions;
