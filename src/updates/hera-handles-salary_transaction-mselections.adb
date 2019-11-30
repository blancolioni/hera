package body Hera.Handles.Salary_Transaction.MSelections is

   function Better_Key
     (Left, Right : Selection_Condition)
      return Boolean;

   function To_Constraints
     (Key : Selection_Key)
      return Selection_Constraint_Lists.List;

   function Time_Stamp_Condition
     (Operation : Constraint_Operator;
      Value     : Hera.Calendar.Time)
      return Selection_Condition;

   function Create_Selection
     (Key : Selection_Key)
      return Hera.Db.Salary_Transaction.Selection;

   function Check_Constraints
     (Constraints : Selection_Constraint_Lists.List;
      Item        : Hera.Db.Salary_Transaction.Salary_Transaction_Type)
      return Boolean;

   type Iterator is
     new Selection_Iterator_Interfaces.Reversible_Iterator with
      record
         Container : access constant Selection;
      end record;

   overriding function First (It : Iterator) return Cursor
   is ((Position => It.Container.Data_Source.First));

   overriding function Last (It : Iterator) return Cursor
   is ((Position => It.Container.Data_Source.Last));

   overriding function Next
     (It   : Iterator;
      Position : Cursor)
      return Cursor
   is ((Position => Element_Lists.Next (Position.Position)));

   overriding function Previous
     (It       : Iterator;
      Position : Cursor)
      return Cursor
   is ((Position => Element_Lists.Previous (Position.Position)));

   ----------
   -- "/=" --
   ----------

   function "/="
     (Left : Time_Stamp_Field; Right : Hera.Calendar.Time)
      return Selection_Condition
   is
      pragma Unreferenced (Left);
   begin
      return Time_Stamp_Condition (Op_NE, Right);
   end "/=";

   ---------
   -- "<" --
   ---------

   function "<"
     (Left : Time_Stamp_Field; Right : Hera.Calendar.Time)
      return Selection_Condition
   is
      pragma Unreferenced (Left);
   begin
      return Time_Stamp_Condition (Op_LT, Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<="
     (Left : Time_Stamp_Field; Right : Hera.Calendar.Time)
      return Selection_Condition
   is
      pragma Unreferenced (Left);
   begin
      return Time_Stamp_Condition (Op_LE, Right);
   end "<=";

   ---------
   -- "=" --
   ---------

   function "="
     (Left  : Time_Stamp_Field;
      Right : Hera.Calendar.Time)
      return Selection_Condition
   is
      pragma Unreferenced (Left);
   begin
      return Time_Stamp_Condition (Op_EQ, Right);
   end "=";

   ---------
   -- ">" --
   ---------

   function ">"
     (Left : Time_Stamp_Field; Right : Hera.Calendar.Time)
      return Selection_Condition
   is
      pragma Unreferenced (Left);
   begin
      return Time_Stamp_Condition (Op_GT, Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">="
     (Left : Time_Stamp_Field; Right : Hera.Calendar.Time)
      return Selection_Condition
   is
      pragma Unreferenced (Left);
   begin
      return Time_Stamp_Condition (Op_GE, Right);
   end ">=";

   -----------
   -- "and" --
   -----------

   function "and"
     (Left, Right : Selection_Condition) return Selection_Condition
   is
      Choose_Left_Key : constant Boolean := Better_Key (Left, Right);
   begin
      return Result : Selection_Condition do
         if Choose_Left_Key then
            Result.Main_Key := Left.Main_Key;
            Result.Constraints := To_Constraints (Right.Main_Key.Element);
         else
            Result.Main_Key := Right.Main_Key;
            Result.Constraints := To_Constraints (Left.Main_Key.Element);
         end if;
         for Constraint of Left.Constraints loop
            Result.Constraints.Append (Constraint);
         end loop;
         for Constraint of Right.Constraints loop
            Result.Constraints.Append (Constraint);
         end loop;
      end return;
   end "and";

   ----------------
   -- Better_Key --
   ----------------

   function Better_Key
     (Left, Right : Selection_Condition)
      return Boolean
   is
      Left_Key  : constant Selection_Key := Left.Main_Key.Element;
      Right_Key : constant Selection_Key := Right.Main_Key.Element;
   begin
      return Right_Key.Key_Type = K_No_Selection_Key
        or else not Right_Key.Has_Value
        or else (Left_Key.Has_Value
                 and then Left_Key.Key_Type >= Right_Key.Key_Type);
   end Better_Key;

   -----------
   -- Buyer --
   -----------

   function Buyer
     (Value : Hera.Handles.Has_Account.Has_Account_Class)
      return Selection_Condition
   is
   begin
      return Selection_Condition'
        (Main_Key    =>
           Selection_Key_Holders.To_Holder
             ((K_Buyer, True, Value.Reference_Has_Account)),
         Constraints => <>);
   end Buyer;

   function Check_Constraints
     (Constraints : Selection_Constraint_Lists.List;
      Item        : Hera.Db.Salary_Transaction.Salary_Transaction_Type)
      return Boolean
   is
   begin
      for Constraint of Constraints loop
         case Constraint.Field is
            when C_Top_Record =>
               declare
                  use Hera.Db;
                  Value : constant Record_Type := Item.Top_Record;
                  Compare : constant Record_Type :=
                    Constraint.Top_Record_Value;
               begin
                  case Constraint.Operator is
                     when Op_None =>
                        raise Constraint_Error with
                          "bad operator: none for field: top_record";
                     when Op_Not =>
                        raise Constraint_Error with
                          "bad operator: not for field: top_record";
                     when Op_EQ =>
                        if Value /= Compare then
                           return False;
                        end if;
                     when Op_NE =>
                        if Value = Compare then
                           return False;
                        end if;
                     when Op_LE =>
                        if Value > Compare then
                           return False;
                        end if;
                     when Op_GT =>
                        if Value <= Compare then
                           return False;
                        end if;
                     when Op_GE =>
                        if Value < Compare then
                           return False;
                        end if;
                     when Op_LT =>
                        if Value >= Compare then
                           return False;
                        end if;
                  end case;
               end;

            when C_Kit_Root_Record =>
               declare
                  use Hera.Db;
                  Value   : constant Kit_Root_Record_Reference :=
                    Item.Get_Kit_Root_Record_Reference;
                  Compare : constant Kit_Root_Record_Reference :=
                    Constraint.Kit_Root_Record_Value;
               begin
                  case Constraint.Operator is
                     when Op_None =>
                        raise Constraint_Error with
                          "bad operator: none for field: kit_root_record";
                     when Op_Not =>
                        raise Constraint_Error with
                          "bad operator: not for field: kit_root_record";
                     when Op_EQ =>
                        if Value /= Compare then
                           return False;
                        end if;
                     when Op_NE =>
                        if Value = Compare then
                           return False;
                        end if;
                     when Op_LE =>
                        raise Constraint_Error with
                          "bad operator: <= for field: kit_root_record";
                     when Op_GT =>
                        raise Constraint_Error with
                          "bad operator: > for field: kit_root_record";
                     when Op_GE =>
                        raise Constraint_Error with
                          "bad operator: >= for field: kit_root_record";
                     when Op_LT =>
                        raise Constraint_Error with
                          "bad operator: < for field: kit_root_record";
                  end case;
               end;

            when C_Time_Stamp =>
               declare
                  use Hera.Calendar;
                  Value   : constant Time := Item.Time_Stamp;
                  Compare : constant Time :=
                    Constraint.Time_Stamp_Value;
               begin
                  case Constraint.Operator is
                     when Op_None =>
                        raise Constraint_Error with
                          "bad operator: none for field: time_stamp";
                     when Op_Not =>
                        raise Constraint_Error with
                          "bad operator: not for field: time_stamp";
                     when Op_EQ =>
                        if Value /= Compare then
                           return False;
                        end if;
                     when Op_NE =>
                        if Value = Compare then
                           return False;
                        end if;
                     when Op_LE =>
                        if Value > Compare then
                           return False;
                        end if;
                     when Op_GT =>
                        if Value <= Compare then
                           return False;
                        end if;
                     when Op_GE =>
                        if Value < Compare then
                           return False;
                        end if;
                     when Op_LT =>
                        if Value >= Compare then
                           return False;
                        end if;
                  end case;
               end;

            when C_Buyer =>
               declare
                  use Hera.Db;
                  Value   : constant Has_Account_Reference :=
                    Item.Buyer;
                  Compare : constant Has_Account_Reference :=
                    Constraint.Buyer_Value;
               begin
                  case Constraint.Operator is
                     when Op_None =>
                        raise Constraint_Error with
                          "bad operator: none for field: buyer";
                     when Op_Not =>
                        raise Constraint_Error with
                          "bad operator: not for field: buyer";
                     when Op_EQ =>
                        if Value /= Compare then
                           return False;
                        end if;
                     when Op_NE =>
                        if Value = Compare then
                           return False;
                        end if;
                     when Op_LE =>
                        raise Constraint_Error with
                          "bad operator: <= for field: buyer";
                     when Op_GT =>
                        raise Constraint_Error with
                          "bad operator: > for field: buyer";
                     when Op_GE =>
                        raise Constraint_Error with
                          "bad operator: >= for field: buyer";
                     when Op_LT =>
                        raise Constraint_Error with
                          "bad operator: < for field: buyer";
                  end case;
               end;

            when C_Seller =>
               declare
                  use Hera.Db;
                  Value   : constant Has_Account_Reference :=
                    Item.Seller;
                  Compare : constant Has_Account_Reference :=
                    Constraint.Seller_Value;
               begin
                  case Constraint.Operator is
                     when Op_None =>
                        raise Constraint_Error with
                          "bad operator: none for field: seller";
                     when Op_Not =>
                        raise Constraint_Error with
                          "bad operator: not for field: seller";
                     when Op_EQ =>
                        if Value /= Compare then
                           return False;
                        end if;
                     when Op_NE =>
                        if Value = Compare then
                           return False;
                        end if;
                     when Op_LE =>
                        raise Constraint_Error with
                          "bad operator: <= for field: seller";
                     when Op_GT =>
                        raise Constraint_Error with
                          "bad operator: > for field: seller";
                     when Op_GE =>
                        raise Constraint_Error with
                          "bad operator: >= for field: seller";
                     when Op_LT =>
                        raise Constraint_Error with
                          "bad operator: < for field: seller";
                  end case;
               end;

            when C_Amount =>
               declare
                  use Hera.Money;
                  Value   : constant Money_Type := Item.Amount;
                  Compare : constant Money_Type :=
                    Constraint.Amount_Value;
               begin
                  case Constraint.Operator is
                     when Op_None =>
                        raise Constraint_Error with
                          "bad operator: none for field: amount";
                     when Op_Not =>
                        raise Constraint_Error with
                          "bad operator: not for field: amount";
                     when Op_EQ =>
                        if Value /= Compare then
                           return False;
                        end if;
                     when Op_NE =>
                        if Value = Compare then
                           return False;
                        end if;
                     when Op_LE =>
                        if Value > Compare then
                           return False;
                        end if;
                     when Op_GT =>
                        if Value <= Compare then
                           return False;
                        end if;
                     when Op_GE =>
                        if Value < Compare then
                           return False;
                        end if;
                     when Op_LT =>
                        if Value >= Compare then
                           return False;
                        end if;
                  end case;
               end;

            when C_Transaction =>
               declare
                  use Hera.Db;
                  Value   : constant Transaction_Reference :=
                    Item.Get_Transaction_Reference;
                  Compare : constant Transaction_Reference :=
                    Constraint.Transaction_Value;
               begin
                  case Constraint.Operator is
                     when Op_None =>
                        raise Constraint_Error with
                          "bad operator: none for field: transaction";
                     when Op_Not =>
                        raise Constraint_Error with
                          "bad operator: not for field: transaction";
                     when Op_EQ =>
                        if Value /= Compare then
                           return False;
                        end if;
                     when Op_NE =>
                        if Value = Compare then
                           return False;
                        end if;
                     when Op_LE =>
                        raise Constraint_Error with
                          "bad operator: <= for field: transaction";
                     when Op_GT =>
                        raise Constraint_Error with
                          "bad operator: > for field: transaction";
                     when Op_GE =>
                        raise Constraint_Error with
                          "bad operator: >= for field: transaction";
                     when Op_LT =>
                        raise Constraint_Error with
                          "bad operator: < for field: transaction";
                  end case;
               end;

         end case;
      end loop;
      return True;
   end Check_Constraints;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased Selection;
      Position  : Cursor)
      return Constant_Reference_Type
   is
   begin
      return (Element =>
                Container.Data_Source.Constant_Reference
                  (Position.Position).Element);
   end Constant_Reference;

   ----------------------
   -- Create_Selection --
   ----------------------

   function Create_Selection
     (Key : Selection_Key)
      return Hera.Db.Salary_Transaction.Selection
   is
   begin
      case Key.Key_Type is
         when K_No_Selection_Key =>
            return Hera.Db.Salary_Transaction.Scan_By_Top_Record;
         when K_Top_Record =>
            if Key.Has_Value then
               return Hera.Db.Salary_Transaction.Select_By_Top_Record
                 (Key.Top_Record_Value);
            else
               return Hera.Db.Salary_Transaction.Scan_By_Top_Record;
            end if;
         when K_Buyer =>
            if Key.Has_Value then
               return Hera.Db.Salary_Transaction.Select_By_Buyer
                 (Key.Buyer_Value);
            else
               return Hera.Db.Salary_Transaction.Scan_By_Buyer;
            end if;
         when K_Seller =>
            if Key.Has_Value then
               return Hera.Db.Salary_Transaction.Select_By_Seller
                 (Key.Seller_Value);
            else
               return Hera.Db.Salary_Transaction.Scan_By_Seller;
            end if;
      end case;
   end Create_Selection;

   -------------
   -- Element --
   -------------

   function Element (Item : Cursor) return Salary_Transaction_Handle is
   begin
      return Element_Lists.Element (Item.Position);
   end Element;

   -----------------
   -- First_Where --
   -----------------

   function First_Where
     (Condition : Selection_Condition'Class) return Salary_Transaction_Handle
   is
      Search : constant Hera.Db.Salary_Transaction.Selection :=
        Create_Selection (Condition.Main_Key.Element);
   begin
      return Result : Salary_Transaction_Handle do
         Result.Reference := Hera.Db.Null_Salary_Transaction_Reference;
         for Item of Search loop
            if Check_Constraints (Condition.Constraints, Item) then
               Result.Reference := Item.Get_Salary_Transaction_Reference;
               exit;
            end if;
         end loop;
      end return;
   end First_Where;

   ---------------
   -- Get_Where --
   ---------------

   function Get_Where
     (Condition : Selection_Condition'Class)
      return Salary_Transaction_Handle
   is
      Search : constant Hera.Db.Salary_Transaction.Selection :=
        Create_Selection (Condition.Main_Key.Element);
   begin
      return Result : Salary_Transaction_Handle do
         Result.Reference :=
           Hera.Db.Salary_Transaction.Element
             (Search.First).Get_Salary_Transaction_Reference;
      end return;
   end Get_Where;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Item : Cursor) return Boolean is
   begin
      return Element_Lists.Has_Element (Item.Position);
   end Has_Element;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Selection) return Boolean is
   begin
      return Container.Data_Source.Is_Empty;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Container : Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class
   is
   begin
      return Iterator'
        (Container => Container'Unchecked_Access);
   end Iterate;

   ----------------
   -- Select_All --
   ----------------

   function Select_All return Selection is
   begin
      return Result : Selection do
         for Item of Hera.Db.Salary_Transaction.Scan_By_Top_Record loop
            Result.Data_Source.Append
              (Get (Item.Get_Salary_Transaction_Reference));
         end loop;
      end return;
   end Select_All;

   ------------------
   -- Select_Where --
   ------------------

   function Select_Where
     (Condition : Selection_Condition'Class) return Selection
   is
   begin
      return Result : Selection do
         for Item of Create_Selection (Condition.Main_Key.Element) loop
            if Check_Constraints (Condition.Constraints, Item) then
               Result.Data_Source.Append
                 (Get (Item.Get_Salary_Transaction_Reference));
            end if;
         end loop;
      end return;
   end Select_Where;

   ----------------
   -- Time_Stamp --
   ----------------

   function Time_Stamp return Time_Stamp_Field is
   begin
      return (null record);
   end Time_Stamp;

   ----------------
   -- Time_Stamp --
   ----------------

   function Time_Stamp (Value : Hera.Calendar.Time) return Selection_Condition
   is
   begin
      return Time_Stamp_Condition (Op_EQ, Value);
   end Time_Stamp;

   --------------------------
   -- Time_Stamp_Condition --
   --------------------------

   function Time_Stamp_Condition
     (Operation : Constraint_Operator;
      Value     : Hera.Calendar.Time)
      return Selection_Condition
   is
   begin
      return Result : Selection_Condition do
         Result.Main_Key :=
           Selection_Key_Holders.To_Holder ((K_No_Selection_Key, False));
         Result.Constraints.Append
           (Selection_Constraint'
              (Field                 => C_Time_Stamp,
               Operator              => Operation,
               Time_Stamp_Value      => Value));
      end return;
   end Time_Stamp_Condition;

   --------------------
   -- To_Constraints --
   --------------------

   function To_Constraints
     (Key : Selection_Key)
      return Selection_Constraint_Lists.List
   is
   begin
      return List : Selection_Constraint_Lists.List do

         if Key.Has_Value then
            case Key.Key_Type is
               when K_No_Selection_Key =>
                  null;
               when K_Top_Record =>
                  null;
               when K_Buyer =>
                  List.Append
                    (Selection_Constraint'
                       (Field                 => C_Buyer,
                        Operator              => Op_EQ,
                        Buyer_Value           => Key.Buyer_Value));
               when K_Seller =>
                  List.Append
                    (Selection_Constraint'
                       (Field                 => C_Seller,
                        Operator              => Op_EQ,
                        Seller_Value           => Key.Seller_Value));
            end case;
         end if;
      end return;
   end To_Constraints;

end Hera.Handles.Salary_Transaction.MSelections;
