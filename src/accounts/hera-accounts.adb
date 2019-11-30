with Ada.Containers.Vectors;

with Hera.Identifiers;

package body Hera.Accounts is

   Account_Version : constant Hera.Objects.Object_Version := "0.0.1";

   use type Hera.Money.Money_Type;

   package Account_Vectors is
     new Ada.Containers.Vectors (Positive, Account_Type);

   function Less (Left, Right : Account_Type) return Boolean
   is (Hera.Money."<" (Left.Current_Cash, Right.Current_Cash));

   package Account_Sorting is
     new Account_Vectors.Generic_Sorting (Less);

   Vector : Account_Vectors.Vector;

   type Cash_Update is new Hera.Objects.Root_Update_Type with
      record
         Transaction : Transaction_Type;
         Change      : Hera.Money.Money_Type;
      end record;

   overriding procedure Execute
     (Update : Cash_Update;
      Target : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : Cash_Update)
      return String
   is ("changed cash "
       & Hera.Money.Show (Update.Change)
       & " for transaction: " & Update.Transaction.Description);

   procedure Change_Cash
     (Account     : Root_Account_Type'Class;
      Transaction : not null access constant Root_Transaction_Type'Class;
      Change      : Hera.Money.Money_Type);

   procedure Save_Transaction
     (Map         : in out Transaction_Maps.Map;
      Transaction : Transaction_Type);

   -----------------
   -- Change_Cash --
   -----------------

   procedure Change_Cash
     (Account     : Root_Account_Type'Class;
      Transaction : not null access constant Root_Transaction_Type'Class;
      Change      : Hera.Money.Money_Type)
   is
   begin
      Account.Add_Update
        (Cash_Update'
           (Hera.Objects.Root_Update_Type with
                Transaction => Transaction_Type (Transaction),
            Change      => Change));
   end Change_Cash;

   ----------
   -- Earn --
   ----------

   procedure Earn
     (Account     : Root_Account_Type'Class;
      Transaction : not null access constant Root_Transaction_Type'Class;
      Amount      : Hera.Money.Money_Type)
   is
   begin
      Account.Change_Cash (Transaction, Amount);
   end Earn;

   ----------
   -- Earn --
   ----------

   procedure Earn
     (Has_Account  : Has_Account_Interface'Class;
      Transaction  : not null access constant Root_Transaction_Type'Class;
      Amount       : Hera.Money.Money_Type)
   is
   begin
      Has_Account.Account.Earn (Transaction, Amount);
   end Earn;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Cash_Update;
      Target : in out Hera.Objects.Root_Hera_Object'Class)
   is
      Account : Root_Account_Type'Class renames
        Root_Account_Type'Class (Target);
   begin
      Save_Transaction (Account.Transactions, Update.Transaction);
      Account.Cash := Account.Cash + Update.Change;
   end Execute;

   -----------------------------
   -- Get_Transaction_Summary --
   -----------------------------

   function Get_Transaction_Summary
     (Account  : Root_Account_Type'Class;
      From, To : Hera.Calendar.Time)
      return Transaction_Summary
   is
      use type Hera.Calendar.Time;
      use type Hera.Identifiers.Object_Identifier;
      use Transaction_Maps;
      Position : Cursor := Account.Transactions.Ceiling (From);
   begin
      return Summary : Transaction_Summary do
         while Has_Element (Position)
           and then Key (Position) <= To
         loop
            for Transaction of Element (Position) loop
               if Summary.Map.Contains (Transaction.Class_Name) then
                  declare
                     Amount : Hera.Money.Money_Type renames
                       Summary.Map (Transaction.Class_Name);
                  begin
                     if Transaction.From.Identifier = Account.Identifier then
                        Amount := Amount - Transaction.Amount;
                     else
                        Amount := Amount + Transaction.Amount;
                     end if;
                  end;
               else
                  Summary.Map.Insert
                    (Transaction.Class_Name,
                     (if Transaction.From.Identifier = Account.Identifier
                      then Hera.Money.Zero - Transaction.Amount
                      else Transaction.Amount));
               end if;
            end loop;
            Next (Position);
         end loop;
      end return;
   end Get_Transaction_Summary;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Summary : Transaction_Summary;
      Process : not null access
        procedure (Class : String;
                   Amount : Hera.Money.Money_Type))
   is
   begin
      for Position in Summary.Map.Iterate loop
         Process (Summary_Maps.Key (Position),
                  Summary_Maps.Element (Position));
      end loop;
   end Iterate;

   ----------------------
   -- Iterate_Accounts --
   ----------------------

   procedure Iterate_Accounts
     (Process : not null access
        procedure (Account : Account_Type))
   is
   begin
      Account_Sorting.Sort (Vector);
      for Account of Vector loop
         Process (Account);
      end loop;
   end Iterate_Accounts;

   --------------------------
   -- Iterate_Transactions --
   --------------------------

   procedure Iterate_Transactions
     (Account : Root_Account_Type'Class;
      Process : not null access
        procedure (Transaction : Transaction_Type))
   is
   begin
      for List of Account.Transactions loop
         for Transaction of List loop
            Process (Transaction);
         end loop;
      end loop;
   end Iterate_Transactions;

   ---------------
   -- Move_Cash --
   ---------------

   procedure Move_Cash
     (From        : Root_Account_Type'Class;
      To          : not null access constant Root_Account_Type'Class;
      Transaction : not null access constant Root_Transaction_Type'Class;
      Proportion  : Unit_Real)
   is
      use Hera.Money;
      Moved : constant Money_Type :=
                Adjust (From.Cash, Proportion);
   begin
      From.Change_Cash (Transaction, Zero - Moved);
      To.Change_Cash (Transaction, Moved);
   end Move_Cash;

   -----------------
   -- New_Account --
   -----------------

   function New_Account
     (Name       : String;
      Start_Cash : Hera.Money.Money_Type)
      return Account_Type
   is
      Account_Object : constant Root_Account_Type := Root_Account_Type'
        (Hera.Objects.Root_Hera_Object with
         Name         => Ada.Strings.Unbounded.To_Unbounded_String (Name),
         Cash         => Start_Cash,
         Transactions => Transaction_Maps.Empty_Map);
   begin
      return Account : constant Account_Type :=
        Account_Type (Account_Object.New_Object (Account_Version))
      do
         Vector.Append (Account);
      end return;
   end New_Account;

   ---------------------
   -- New_Transaction --
   ---------------------

   procedure New_Transaction
     (Transaction : in out Root_Transaction_Type'Class;
      From, To    : Account_Type;
      Amount      : Hera.Money.Money_Type)
   is
   begin
      Transaction.From := From;
      Transaction.To   := To;
      Transaction.Amount := Amount;
   end New_Transaction;

   ----------------------
   -- Save_Transaction --
   ----------------------

   procedure Save_Transaction
     (Map         : in out Transaction_Maps.Map;
      Transaction : Transaction_Type)
   is
      Key : constant Hera.Calendar.Time := Hera.Calendar.Clock;
   begin
      if not Map.Contains (Key) then
         Map.Insert (Key, Transaction_Lists.Empty_List);
      end if;

      Map (Key).Append (Transaction);
   end Save_Transaction;

   ---------------
   -- Serialize --
   ---------------

   overriding procedure Serialize
     (Account         : Root_Account_Type;
      Detail          : Hera.Serialization.Detail_Level_Type;
      Knowledge       : Hera.Knowledge.Knowledge_Interface'Class;
      Json            : in out Hera.Json.Json_Object'Class)
   is
   begin
      Hera.Objects.Root_Hera_Object (Account).Serialize
        (Detail, Knowledge, Json);
      Account.Set_Property
        (Json, Knowledge, Hera.Knowledge.Exists,
         "name", Ada.Strings.Unbounded.To_String (Account.Name));
      Account.Set_Property
        (Json, Knowledge, Hera.Knowledge.Full,
         "cash", Account.Current_Cash);
   end Serialize;

   -----------
   -- Spend --
   -----------

   procedure Spend
     (Account     : Root_Account_Type'Class;
      Transaction : not null access constant Root_Transaction_Type'Class;
      Amount      : Hera.Money.Money_Type)
   is
   begin
      Account.Change_Cash (Transaction, Hera.Money.Zero - Amount);
   end Spend;

   -----------
   -- Spend --
   -----------

   procedure Spend
     (Has_Account : Has_Account_Interface'Class;
      Transaction : not null access constant Root_Transaction_Type'Class;
      Amount      : Hera.Money.Money_Type)
   is
   begin
      Has_Account.Account.Spend (Transaction, Amount);
   end Spend;

end Hera.Accounts;
