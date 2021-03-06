with Ada.Integer_Text_Io;
with Ada.Text_Io;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_Io;

use Ada.Integer_Text_Io;
use Ada.Text_Io;
use Ada.Characters.Handling;
use Ada.Strings.Unbounded;
use Ada.Strings.Unbounded.Text_Io;

procedure Scheme is

   procedure Stderr (Str: in String) is
   begin
      Set_Output(Standard_Error);
      Put("Error: ");
      Put(Str);
      New_Line;
   end;

   -- MODEL ---------------------------------------------------------------

   type Object_Type is (Int, Bool, Char, Strng, Empty_List, Pair, Symbol,
                        Primitive_Proc, Compound_Proc);

   type Object;
   type Access_Object is access Object;
   type Access_Function is access function (Args : Access_Object) return Access_Object;

   type Pair_Object is record
      Car : Access_Object;
      Cdr : Access_Object;
   end record;

   type Compound_Proc_Object is record
      Parameters : Access_Object;
      L_Body : Access_Object;
      Env : Access_Object;
   end record;

   type Object_Data is record
      Int : Integer;
      Bool : Boolean;
      Char : Character;
      Strng : Unbounded_String;
      Pair : Pair_Object;
      Symbol : Unbounded_String;
      Primitive : Access_Function;
      Compound_Proc : Compound_Proc_Object;
   end record;

   type Object is record
      O_Type : Object_Type;
      Data : Object_Data;
   end record;

   function Alloc_Object return Access_Object is
      Obj : Access_Object;
   begin
      Obj := new Object;
      return Obj;
   end;

   -- Useful for debugging to declare this first so it can be used elsewhere.
   procedure Print (Obj : in Access_Object);

   True_Singleton : Access_Object;
   False_Singleton : Access_Object;
   The_Empty_List : Access_Object;
   Symbol_Table : Access_Object;
   Quote_Symbol : Access_Object;
   Define_Symbol : Access_Object;
   Set_Symbol : Access_Object;
   Ok_Symbol : Access_Object;
   If_Symbol : Access_Object;
   Lambda_Symbol : Access_Object;
   Lambda_Char_Symbol : Access_Object;
   Begin_Symbol : Access_Object;
   Cond_Symbol : Access_Object;
   Else_Symbol : Access_Object;
   Let_Symbol : Access_Object;
   And_Symbol : Access_Object;
   Or_Symbol : Access_Object;
   The_Empty_Environment : Access_Object;
   The_Global_Environment : Access_Object;

   function Is_Boolean (Obj : Access_Object) return Boolean is
   begin
      return Obj.all.O_Type = Bool;
   end;

   function Is_False (Obj : Access_Object) return Boolean is
   begin
      return Obj = False_Singleton;
   end;

   function Is_True (Obj : Access_Object) return Boolean is
   begin
      return Obj = True_Singleton;
   end;

   function Is_The_Empty_List (Obj : Access_Object) return Boolean is
   begin
      return Obj = The_Empty_List;
   end;

   function Is_Integer (Obj : Access_Object) return Boolean is
   begin
      return Obj.all.O_Type = Int;
   end;

   function Is_Character (Obj : Access_Object) return Boolean is
   begin
      return Obj.all.O_Type = Char;
   end;

   function Is_String (Obj : Access_Object) return Boolean is
   begin
      return Obj.all.O_Type = Strng;
   end;

   function Is_Pair (Obj : Access_Object) return Boolean is
   begin
      return Obj.all.O_Type = Pair;
   end;

   function Is_Symbol (Obj : Access_Object) return Boolean is
   begin
      return Obj.all.O_Type = Symbol;
   end;

   function Cons (Car : Access_Object;
                  Cdr : Access_Object) return Access_Object is
      Obj : Access_Object;
   begin
      Obj := Alloc_Object;
      Obj.all.O_Type := Pair;
      Obj.all.Data.Pair.Car := Car;
      Obj.all.Data.Pair.Cdr := Cdr;
      return Obj;
   end;

   function Car (Pair_Obj : Access_Object) return Access_Object is
   begin
      return Pair_Obj.all.Data.Pair.Car;
   end;

   procedure Set_Car (Pair_Obj : in Access_Object;
                      Val : in Access_Object) is
   begin
      Pair_Obj.all.Data.Pair.Car := Val;
   end;

   function Cdr (Pair_Obj : Access_Object) return Access_Object is
   begin
      return Pair_Obj.all.Data.Pair.Cdr;
   end;

   procedure Set_Cdr (Pair_Obj : in Access_Object;
                      Val : in Access_Object) is
   begin
      Pair_Obj.all.Data.Pair.Cdr := Val;
   end;

   -- Thank the RMS for keyboard macros... How else would I have made all of
   --  these car/cdr combos??
   function caar(Obj : Access_Object) return Access_Object is
   begin
      return car(car(obj));
   end;

   function cadr(Obj : Access_Object) return Access_Object is
   begin
      return car(cdr(obj));
   end;

   function cdar(Obj : Access_Object) return Access_Object is
   begin
      return cdr(car(obj));
   end;

   function cddr(Obj : Access_Object) return Access_Object is
   begin
      return cdr(cdr(obj));
   end;

   function caaar(Obj : Access_Object) return Access_Object is
   begin
      return car(car(car(obj)));
   end;

   function caadr(Obj : Access_Object) return Access_Object is
   begin
      return car(car(cdr(obj)));
   end;

   function cadar(Obj : Access_Object) return Access_Object is
   begin
      return car(cdr(car(obj)));
   end;

   function caddr(Obj : Access_Object) return Access_Object is
   begin
      return car(cdr(cdr(obj)));
   end;

   function cdaar(Obj : Access_Object) return Access_Object is
   begin
      return cdr(car(car(obj)));
   end;

   function cdadr(Obj : Access_Object) return Access_Object is
   begin
      return cdr(car(cdr(obj)));
   end;

   function cddar(Obj : Access_Object) return Access_Object is
   begin
      return cdr(cdr(car(obj)));
   end;

   function cdddr(Obj : Access_Object) return Access_Object is
   begin
      return cdr(cdr(cdr(obj)));
   end;

   function caaaar(Obj : Access_Object) return Access_Object is
   begin
      return car(car(car(car(obj))));
   end;

   function caaadr(Obj : Access_Object) return Access_Object is
   begin
      return car(car(car(cdr(obj))));
   end;

   function caadar(Obj : Access_Object) return Access_Object is
   begin
      return car(car(cdr(car(obj))));
   end;

   function caaddr(Obj : Access_Object) return Access_Object is
   begin
      return car(car(cdr(cdr(obj))));
   end;

   function cadaar(Obj : Access_Object) return Access_Object is
   begin
      return car(cdr(car(car(obj))));
   end;

   function cadadr(Obj : Access_Object) return Access_Object is
   begin
      return car(cdr(car(cdr(obj))));
   end;

   function caddar(Obj : Access_Object) return Access_Object is
   begin
      return car(cdr(cdr(car(obj))));
   end;

   function cadddr(Obj : Access_Object) return Access_Object is
   begin
      return car(cdr(cdr(cdr(obj))));
   end;

   function cdaaar(Obj : Access_Object) return Access_Object is
   begin
      return cdr(car(car(car(obj))));
   end;

   function cdaadr(Obj : Access_Object) return Access_Object is
   begin
      return cdr(car(car(cdr(obj))));
   end;

   function cdadar(Obj : Access_Object) return Access_Object is
   begin
      return cdr(car(cdr(car(obj))));
   end;

   function cdaddr(Obj : Access_Object) return Access_Object is
   begin
      return cdr(car(cdr(cdr(obj))));
   end;

   function cddaar(Obj : Access_Object) return Access_Object is
   begin
      return cdr(cdr(car(car(obj))));
   end;

   function cddadr(Obj : Access_Object) return Access_Object is
   begin
      return cdr(cdr(car(cdr(obj))));
   end;

   function cdddar(Obj : Access_Object) return Access_Object is
   begin
      return cdr(cdr(cdr(car(obj))));
   end;

   function cddddr(Obj : Access_Object) return Access_Object is
   begin
      return cdr(cdr(cdr(cdr(obj))));
   end;

   function Make_Compound_Proc (Parameters : Access_Object;
                                L_Body : Access_Object;
                                Env : Access_Object) return Access_Object is
      Obj : Access_Object;
   begin
      Obj := Alloc_Object;
      Obj.all.O_Type := Compound_Proc;
      Obj.all.Data.Compound_Proc.Parameters := Parameters;
      Obj.all.Data.Compound_Proc.L_Body := L_Body;
      Obj.all.Data.Compound_Proc.Env := Env;
      return Obj;
   end;

   function Is_Compound_Proc (Obj : Access_Object) return Boolean is
   begin
      return Obj.all.O_Type = Compound_Proc;
   end;

   function Enclosing_Environment (Env : Access_Object) return Access_Object is
   begin
      return Cdr(Env);
   end;

   function First_Frame (Env : Access_Object) return Access_Object is
   begin
      return Car(Env);
   end;

   function Make_Frame (Vars : Access_Object;
                        Vals : Access_Object) return Access_Object is
   begin
      return Cons(Vars, Vals);
   end;

   function Frame_Variables (Frame : Access_Object) return Access_Object is
   begin
      return Car(Frame);
   end;

   function Frame_Values (Frame : Access_Object) return Access_Object is
   begin
      return Cdr(Frame);
   end;

   procedure Add_Binding_To_Frame (Var : in Access_Object;
                                   Val : in Access_Object;
                                   Frame : in Access_Object) is
   begin
      Set_Car(Frame, Cons(Var, Car(Frame)));
      Set_Cdr(Frame, Cons(Val, Cdr(Frame)));
   end;

   function Extend_Environment (Vars : Access_Object;
                                Vals : Access_Object;
                                Base_Env : Access_Object) return Access_Object is
   begin
      return Cons(Make_Frame(Vars, Vals), Base_Env);
   end;

   function Lookup_Variable_Value (Var : Access_Object;
                                   Env : Access_Object) return Access_Object is
      Frame, Vars, Vals : Access_Object;
      This_Env : Access_Object := Env;
   begin
      while This_Env /= The_Empty_List loop
         Frame := First_Frame(This_Env);
         Vars := Frame_Variables(Frame);
         Vals := Frame_Values(Frame);
         while Vars /= The_Empty_List loop
            if Var = Car(Vars) then
               return Car(Vals);
            else
               Vars := Cdr(Vars);
               Vals := Cdr(Vals);
            end if;
         end loop;
         This_Env := Enclosing_Environment(This_Env);
      end loop;
      Stderr("Unbound variable '" & To_String(Var.all.Data.Symbol) & "'");
      raise Constraint_Error;
   end;

   procedure Set_Variable_Value (Var : in Access_Object;
                                 Val : in Access_Object;
                                 Env : in Access_Object) is
      Frame, Vars, Vals : Access_Object;
      This_Env : Access_Object := Env;
   begin
      while This_Env /= The_Empty_List loop
         Frame := First_Frame(This_Env);
         Vars := Frame_Variables(Frame);
         Vals := Frame_Values(Frame);
         while Vars /= The_Empty_List loop
            if Var = Car(Vars) then
               Set_Car(Vals, Val);
               return;
            end if;
            Vars := Cdr(Vars);
            Vals := Cdr(Vals);
         end loop;
         This_Env := Enclosing_Environment(This_Env);
      end loop;
      Stderr("Unbound variable '" & To_String(Var.all.Data.Symbol) & "' must be defined before set.");
      raise Constraint_Error;
   end;

   procedure Define_Variable (Var : in Access_Object;
                              Val : in Access_Object;
                              Env : in Access_Object) is
      Frame, Vars, Vals : Access_Object;
   begin
      Frame := First_Frame(Env);
      Vars := Frame_Variables(Frame);
      Vals := Frame_Values(Frame);
      while Vars /= The_Empty_List loop
         if Var = Car(Vars) then
            Set_Car(Vals, Val);
            return;
         end if;
         Vars := Cdr(Vars);
         Vals := Cdr(Vals);
      end loop;
      Add_Binding_To_Frame(Var, Val, Frame);
   end;

   function Interaction_Environment_Proc (Arguments : Access_Object) return Access_Object is
   begin
      return The_Global_Environment;
   end;

   function Setup_Environment return Access_Object is
      Initial_Env : Access_Object;
   begin
      Initial_Env := Extend_Environment(The_Empty_List,
                                        The_Empty_List,
                                        The_Empty_Environment);
      return Initial_Env;
   end;

   function Null_Environment_Proc (Arguments : Access_Object) return Access_Object is
   begin
      return Setup_Environment;
   end;

   function Populate_Environment (Env : Access_Object) return Access_Object;

   function Make_Environment return Access_Object is
      Env : Access_Object := Populate_Environment(Setup_Environment);
   begin
      return Env;
   end;

   function Environment_Proc (Args : Access_Object) return Access_Object is
   begin
      return Make_Environment;
   end;

   function Make_Symbol (Value : Unbounded_String) return Access_Object is
      Obj : Access_Object;
      Element : Access_Object;

      function Same_Str(One, Two : Unbounded_String) return Boolean is
      begin
         return Length(One) = Length(Two) and then One = Two;
      end;

   begin
      -- Search for the symbol in the symbol table.
      Element := Symbol_Table;
      while not Is_The_Empty_List(Element) loop
         if Same_Str(Car(Element).all.Data.Symbol, Value) then
            return Car(Element);
         end if;
         Element := Cdr(Element);
      end loop;

      Obj := Alloc_Object;
      Obj.all.O_Type := Symbol;
      Obj.all.Data.Symbol := Value;
      Symbol_Table := Cons(Obj, Symbol_Table);
      return Obj;
   end;

   function Make_Integer (Value : Integer) return Access_Object is
      Obj : Access_Object;
   begin
      Obj := Alloc_Object;
      Obj.all.O_Type := Int;
      Obj.all.Data.Int := Value;
      return Obj;
   end;

   function Make_Char (C : Character) return Access_Object is
      Obj : Access_Object;
   begin
      Obj := Alloc_Object;
      Obj.all.O_Type := Char;
      Obj.all.Data.Char := C;
      return Obj;
   end;

   function Make_String (Str : Unbounded_String) return Access_Object is
      Obj : Access_Object;
   begin
      Obj := Alloc_Object;
      Obj.all.O_Type := Strng;
      Obj.all.Data.Strng := Str;
      return Obj;
   end;

   function Make_Begin (Expr : Access_Object) return Access_Object is
   begin
      return Cons(Begin_Symbol, Expr);
   end;

   function Make_Primitive_Proc (Fn : Access_Function) return Access_Object is
      Obj : Access_Object;
   begin
      Obj := Alloc_Object;
      Obj.all.O_Type := Primitive_Proc;
      Obj.all.Data.Primitive := Fn;
      return Obj;
   end;

   function Is_Primitive_Proc (Obj : Access_Object) return Boolean is
   begin
      return Obj.all.O_Type = Primitive_Proc;
   end;

   function Add_Proc (Arguments : Access_Object) return Access_Object is
      Result : Integer := 0;
      Args : Access_Object := Arguments;
   begin
      loop
         exit when Is_The_Empty_List(Args);
         Result := Result + Car(Args).all.Data.Int;
         Args := Cdr(Args);
      end loop;

      return Make_Integer(Result);
   end;

   function Sub_Proc (Arguments : Access_Object) return Access_Object is
      Result : Integer := Car(Arguments).all.Data.Int;
      Args : Access_Object := Cdr(Arguments);
   begin
      if Is_The_Empty_List(Args) then
         return Make_Integer(Result * (-1));
      else
         loop
            exit when Is_The_Empty_List(Args);
            Result := Result - Car(Args).all.Data.Int;
            Args := Cdr(Args);
         end loop;
         return Make_Integer(Result);
      end if;
   end;

   function Multiply_Proc (Arguments : Access_Object) return Access_Object is
      Result : Integer := 1;
      Args : Access_Object := Arguments;
   begin
      loop
         exit when Is_The_Empty_List(Args);
         Result := Result * Car(Args).all.Data.Int;
         Args := Cdr(Args);
      end loop;
      return Make_Integer(Result);
   end;

   function Quotient_Proc (Arguments : Access_Object) return Access_Object is
   begin
      return Make_Integer(Car(Arguments).all.Data.Int /
                            Cadr(Arguments).all.Data.Int);
   end;

   function Remainder_Proc (Arguments : Access_Object) return Access_Object is
   begin
      return Make_Integer(Car(Arguments).all.Data.Int rem
                            Cadr(Arguments).all.Data.Int);
   end;

   function Equal_Proc (Arguments : Access_Object) return Access_Object is
      Last_Val : Integer := Car(Arguments).all.Data.Int;
      Args : Access_Object := Cdr(Arguments);
   begin
      loop
         if Is_The_Empty_List(Args) then
            return True_Singleton;
         else
            if not (Car(Args).all.Data.Int = Last_Val) then
               return False_Singleton;
            end if;
            Last_Val := Car(Args).all.Data.Int;
            Args := Cdr(Args);
         end if;
      end loop;
   end;

   function Lt_Proc (Arguments : Access_Object) return Access_Object is
      Last_Val : Integer := Car(Arguments).all.Data.Int;
      Args : Access_Object := Cdr(Arguments);
   begin
      loop
         if Is_The_Empty_List(Args) then
            return True_Singleton;
         else
            if not (Last_Val < Car(Args).all.Data.Int) then
               return False_Singleton;
            end if;
            Last_Val := Car(Args).all.Data.Int;
            Args := Cdr(Args);
         end if;
      end loop;
   end;

   function Gt_Proc (Arguments : Access_Object) return Access_Object is
      Last_Val : Integer := Car(Arguments).all.Data.Int;
      Args : Access_Object := Cdr(Arguments);
   begin
      loop
         if Is_The_Empty_List(Args) then
            return True_Singleton;
         else
            if not (Last_Val > Car(Args).all.Data.Int) then
               return False_Singleton;
            end if;
            Last_Val := Car(Args).all.Data.Int;
            Args := Cdr(Args);
         end if;
      end loop;
   end;

   function Is_Null_Proc (Arguments : Access_Object) return Access_Object is
   begin
      if Is_The_Empty_List(Car(Arguments)) then
         return True_Singleton;
      else
         return False_Singleton;
      end if;
   end;

   function Is_Boolean_Proc (Arguments : Access_Object) return Access_Object is
      Obj : Access_Object := Car(Arguments);
   begin
      if Is_Boolean(Obj) then
         return True_Singleton;
      else
         return False_Singleton;
      end if;
   end;

   function Is_Symbol_Proc (Arguments : Access_Object) return Access_Object is
      Obj : Access_Object := Car(Arguments);
   begin
      if Is_Symbol(obj) then
         return True_Singleton;
      else
         return False_Singleton;
      end if;
   end;

   function Is_Integer_Proc (Arguments : Access_Object) return Access_Object is
      Obj : Access_Object := Car(Arguments);
   begin
      if Is_Integer(Obj) then
         return True_Singleton;
      else
         return False_Singleton;
      end if;
   end;

   function Is_Char_Proc (Arguments : Access_Object) return Access_Object is
      Obj : Access_Object := Car(Arguments);
   begin
      if Is_Character(Obj) then
         return True_Singleton;
      else
         return False_Singleton;
      end if;
   end;

   function Is_String_Proc (Arguments : Access_Object) return Access_Object is
      Obj : Access_Object := Car(Arguments);
   begin
      if Is_String(Obj) then
         return True_Singleton;
      else
         return False_Singleton;
      end if;
   end;

   function Is_Procedure_Proc (Arguments : Access_Object) return Access_Object is
      Obj : Access_Object := Car(Arguments);
   begin
      if Is_Primitive_Proc(Obj) or else Is_Compound_Proc(Obj) then
         return True_Singleton;
      else
         return False_Singleton;
      end if;
   end;

   function Is_Pair_Proc (Arguments : Access_Object) return Access_Object is
      Obj : Access_Object := Car(Arguments);
   begin
      if Is_Pair(Obj) then
         return True_Singleton;
      else
         return False_Singleton;
      end if;
   end;

   function Cons_Proc (Arguments : Access_Object) return Access_Object is
      Car_Obj : Access_Object := Car(Arguments);
      Cdr_Obj : Access_Object := Cadr(Arguments);
   begin
      return Cons(Car_Obj, Cdr_Obj);
   end;

   function Car_Proc (Arguments : Access_Object) return Access_Object is
      List_Obj : Access_Object := Car(Arguments);
   begin
      return Car(List_Obj);
   end;

   function Cdr_Proc (Arguments : Access_Object) return Access_Object is
      List_Obj : Access_Object := Car(Arguments);
   begin
      return Cdr(List_Obj);
   end;

   function Set_Car_Proc (Arguments : Access_Object) return Access_Object is
   begin
      Set_Car(Car(Arguments), Cadr(Arguments));
      return Ok_Symbol;
   end;

   function Set_Cdr_Proc (Arguments : Access_Object) return Access_Object is
   begin
      Set_Cdr(Car(Arguments), Cadr(Arguments));
      return Ok_Symbol;
   end;

   function List_Proc (Arguments : Access_Object) return Access_Object is
   begin
      return Arguments;
   end;

   function Eq_Proc (Arguments : Access_Object) return Access_Object is
      Result : Boolean := True;
      Last : Access_Object := Car(Arguments);
      Args : Access_Object := Cdr(Arguments);
   begin
      loop
         if Is_The_Empty_List(Args) then
            return True_Singleton;
         else
            if not (Last.all.O_Type = Car(Args).all.O_Type) then
               return False_Singleton;
            else
               case Last.all.O_Type is
                  when Int =>
                     if Last.all.Data.Int /= Car(Args).all.Data.Int then
                        return False_Singleton;
                     end if;
                  when Char =>
                     if Last.all.Data.Char /= Car(Args).all.Data.Char then
                        return False_Singleton;
                     end if;
                  when Strng =>
                     if Last.all.Data.Strng /= Car(Args).all.Data.Strng then
                        return False_Singleton;
                     end if;
                  when others =>
                     if Last /= Car(Args) then
                        return False_Singleton;
                     end if;
               end case;
               Last := Car(Args);
               Args := Cdr(Args);
            end if;
         end if;
      end loop;
   end;

   function Char_To_Int_Proc (Arguments : Access_Object) return Access_Object is
   begin
      return Make_Integer(Character'Pos(Car(Arguments).all.Data.Char));
   end;

   function Int_To_Char_Proc (Arguments : Access_Object) return Access_Object is
   begin
      return Make_Char(Character'Val(Car(Arguments).all.Data.Int));
   end;

   function Number_To_String_Proc (Arguments : Access_Object) return Access_Object is
      Str : Unbounded_String := To_Unbounded_String(Integer'Image(Car(Arguments).all.Data.Int));
   begin
      return Make_String(To_Unbounded_String(Slice(Str, 2, Length(Str))));
   end;

   function String_To_Number_Proc (Arguments : Access_Object) return Access_Object is
   begin
      return Make_Integer(Integer'Value(To_String(Car(Arguments).all.Data.Strng)));
   end;

   function Symbol_To_String_Proc (Arguments : Access_Object) return Access_Object is
   begin
      return Make_String(Car(Arguments).all.Data.Symbol);
   end;

   function String_To_Symbol_Proc (Arguments : Access_Object) return Access_Object is
   begin
      return Make_Symbol(Car(Arguments).all.Data.Strng);
   end;

   function Apply_Proc (Arguments : Access_Object) return Access_Object is
   begin
      Stderr("This should never be called because it is a hack.");
      raise Constraint_Error;
      return False_Singleton;
   end;

   function Eval_Proc (Arguments : Access_Object) return Access_Object is
   begin
      Stderr("This should never be called because it is a hack.");
      raise Constraint_Error;
      return False_Singleton;
   end;

   procedure Init is
   begin
      The_Empty_List := Alloc_Object;
      The_Empty_List.all.O_Type := Empty_List;

      False_Singleton := Alloc_Object;
      False_Singleton.all.O_Type := Bool;
      False_Singleton.all.Data.Bool := False;

      True_Singleton := Alloc_Object;
      True_Singleton.all.O_Type := Bool;
      True_Singleton.all.Data.Bool := True;

      Symbol_Table := The_Empty_List;
      Quote_Symbol := Make_Symbol(To_Unbounded_String("quote"));
      Define_Symbol := Make_Symbol(To_Unbounded_String("define"));
      Set_Symbol := Make_Symbol(To_Unbounded_String("set!"));
      Ok_Symbol := Make_Symbol(To_Unbounded_String("ok"));
      If_Symbol := Make_Symbol(To_Unbounded_String("if"));
      Lambda_Symbol := Make_Symbol(To_Unbounded_String("lambda"));
      Lambda_Char_Symbol := Make_Symbol(To_Unbounded_String("λ"));
      Begin_Symbol := Make_Symbol(To_Unbounded_String("begin"));
      Cond_Symbol := Make_Symbol(To_Unbounded_String("cond"));
      Else_Symbol := Make_Symbol(To_Unbounded_String("else"));
      Let_Symbol := Make_Symbol(To_Unbounded_String("let"));
      And_Symbol := Make_Symbol(To_Unbounded_String("and"));
      Or_Symbol := Make_Symbol(To_Unbounded_String("or"));

      The_Empty_Environment := The_Empty_List;
      The_Global_Environment := Make_Environment;
   end;

   function Populate_Environment (Env : Access_Object) return Access_Object is
      procedure Def_Primitive_Proc (Symbol : in String;
                                    Proc : in Access_Function;
                                    Env : in Access_Object) is
      begin
         Define_Variable(Make_Symbol(To_Unbounded_String(Symbol)),
                         Make_Primitive_Proc(Proc),
                         Env);

      end;
   begin
      Def_Primitive_Proc("+", Add_Proc'Access, Env);
      Def_Primitive_Proc("-", Sub_Proc'Access, Env);
      Def_Primitive_Proc("*", Multiply_Proc'Access, Env);
      Def_Primitive_Proc("quotient", Quotient_Proc'Access, Env);
      Def_Primitive_Proc("remainder", Remainder_Proc'Access, Env);
      Def_Primitive_Proc("=", Equal_Proc'Access, Env);
      Def_Primitive_Proc("<", Lt_Proc'Access, Env);
      Def_Primitive_Proc(">", Gt_Proc'Access, Env);
      Def_Primitive_Proc("null?", Is_Null_Proc'Access, Env);
      Def_Primitive_Proc("cons", Cons_Proc'Access, Env);
      Def_Primitive_Proc("car", Car_Proc'Access, Env);
      Def_Primitive_Proc("cdr", Cdr_Proc'Access, Env);
      Def_Primitive_Proc("set-car!", Set_Car_Proc'Access, Env);
      Def_Primitive_Proc("set-cdr!", Set_Cdr_Proc'Access, Env);
      Def_Primitive_Proc("list", List_Proc'Access, Env);
      Def_Primitive_Proc("boolean?", Is_Boolean_Proc'Access, Env);
      Def_Primitive_Proc("symbol?", Is_Symbol_Proc'Access, Env);
      Def_Primitive_Proc("integer?", Is_Integer_Proc'Access, Env);
      Def_Primitive_Proc("char?", Is_Char_Proc'Access, Env);
      Def_Primitive_Proc("string?", Is_String_Proc'Access, Env);
      Def_Primitive_Proc("pair?", Is_Pair_Proc'Access, Env);
      Def_Primitive_Proc("procedure?", Is_Procedure_Proc'Access, Env);
      Def_Primitive_Proc("eq?", Eq_Proc'Access, Env);
      Def_Primitive_Proc("char->integer", Char_To_Int_Proc'Access, Env);
      Def_Primitive_Proc("integer->char", Int_To_Char_Proc'Access, Env);
      Def_Primitive_Proc("number->string", Number_To_String_Proc'Access, Env);
      Def_Primitive_Proc("string->number", String_To_Number_Proc'Access, Env);
      Def_Primitive_Proc("symbol->string", Symbol_To_String_Proc'Access, Env);
      Def_Primitive_Proc("string->symbol", String_To_Symbol_Proc'Access, Env);
      Def_Primitive_Proc("apply", Apply_Proc'Access, Env);
      Def_Primitive_Proc("interaction-environment", Interaction_Environment_Proc'Access, Env);
      Def_Primitive_Proc("null-environment", Null_Environment_Proc'Access, Env);
      Def_Primitive_Proc("environment", Environment_Proc'Access, Env);
      Def_Primitive_Proc("eval", Eval_Proc'Access, Env);

      return Env;
   end;

   -- READ ----------------------------------------------------------------

   procedure Read (Str : in out Unbounded_String;
                   Obj : out Access_Object) is

      procedure Read_From_Index (Str : in out Unbounded_String;
                                 I : in out Integer;
                                 Obj : in out Access_Object);

      I : Integer := 1;

      procedure Eat_Whitespace (Str : in out Unbounded_String;
                                I : in out Integer) is
      begin
         loop
            begin
               exit when Element(Str, I) /= ' ';
               I := I + 1;
            exception
               when Ada.Strings.Index_Error =>
                  Get_Line(Str);
                  I := 1;
            end;
         end loop;
      end;

      function Is_Delimiter (C : Character) return Boolean is
      begin
         return C = ' ' or else C = '('
           or else C = ')' or else C = '"';
      end;

      function Is_Space (C : Character) return Boolean is
      begin
         return C = ' ';
      end;

      function Is_Initial (C : Character) return Boolean is
      begin
         return Is_Alphanumeric(C) or else C = '*' or else
           C = '/' or else C = '>' or else
           C = '<' or else C = '=' or else
           C = '?' or else C = '!';
      end;

      procedure Read_String (Str : in out Unbounded_String;
                             Obj_Str : in out Unbounded_String;
                             I : in out Integer) is
      begin
         loop
            begin
               exit when Element(Str, I) = '"';

               if Element(Str, I) = '\' then
                  I := I + 1;
                  case Element(Str, I) is
                     when 'n' =>
                        Append(Obj_Str, Character'Val(10));
                     when '"' =>
                        Append(Obj_Str, '"');
                     when '\' =>
                        Append(Obj_Str, '\');
                     when others =>
                        Stderr("Unkown character escape.");
                        raise Constraint_Error;
                  end case;
               else
                  Append(Obj_Str, Slice(Str, I, I));
               end if;
               I := I + 1;

            exception
               when Ada.Strings.Index_Error =>
                  Append(Obj_Str, Character'Val(10));
                  Str := Get_Line;
                  I := 1;
            end;
         end loop;
         I := I + 1;
      end;

      procedure Read_Integer (Str : in out Unbounded_String;
                              I : in out Integer;
                              Obj : in out Access_Object) is
         Sign : Integer := 1;
         Num : Integer := 0;
      begin
         if Element(Str, I) = '-' then
            Sign := -1;
            I := I + 1;
         end if;

         while Length(Str) >= I and then Is_Digit(Element(Str, I)) loop
            Num := (Num * 10);
            Num := Num + (Character'Pos(Element(Str, I)) - Character'Pos('0'));
            I := I + 1;
         end loop;
         Num := Num * Sign;

         if I = Length(Str) + 1 or else Is_Delimiter(Element(Str, I)) then
            Obj := Make_Integer(Num);
            return;
         else
            Stderr("Number not followed by a delimiter.");
            raise Constraint_Error;
         end if;
      end;

      procedure Read_Character (Str : in out Unbounded_String;
                                I : in out Integer;
                                Obj : in out Access_Object) is
      begin
         -- Check for "#\space" and "#\newline"
         begin
            if Element(Str, I) = 's' then
               if Slice(Str, I, I + 4) = "space" then
                  Obj := Make_Char(' ');
                  I := I + 5;
                  return;
               end if;
            elsif Element(Str, I) = 'n' then
               if Slice(Str, I, I + 6) = "newline" then
                  Obj := Make_Char(Character'Val(10));
                  I := I + 7;
                  return;
               end if;
            end if;
         exception
            when Ada.Strings.Index_Error =>
               null;
         end;

         -- If the index fails, that means a newline was entered since Ada
         -- won't keep the last \n.
         begin
            Obj := Make_Char(Element(Str, I));
            I := I + 1;
            return;
         exception
            when Ada.Strings.Index_Error =>
               Obj := Make_Char(Character'Val(10));
               return;
         end;
      end;

      procedure Read_Pair (Str : in out Unbounded_String;
                           I : in out Integer;
                           Obj : in out Access_Object) is
         Car_Obj : Access_Object;
         Cdr_Obj : Access_Object;
      begin
         Eat_Whitespace(Str, I);

         if Element(Str, I) = ')' then
            loop
               begin
                  Obj := The_Empty_List;
                  I := I + 1;
                  return;
               exception
                  when Ada.Strings.Index_Error =>
                     Get_Line(Str);
                     I := 1;
               end;
            end loop;
         end if;

         Read_From_Index(Str, I, Car_Obj);
         Eat_Whitespace(Str, I);

         loop
            begin
               if Element(Str, I) = '.' then
                  -- Improper list
                  loop
                     begin
                        I := I + 1;
                        Eat_Whitespace(Str, I);
                        Read_From_Index(Str, I, Cdr_Obj);

                        Eat_Whitespace(Str, I);
                        if Element(Str, I) /= ')' then
                           Stderr("No trailing paren after pair.");
                           raise Constraint_Error;
                        end if;

                        Obj := Cons(Car_Obj, Cdr_Obj);
                        I := I + 1;
                        return;
                     exception
                        when Ada.Strings.Index_Error =>
                           Get_Line(Str);
                           I := 1;
                     end;
                  end loop;

               else
                  -- Proper list
                  loop
                     begin
                        Read_Pair(Str, I, Cdr_Obj);
                        Obj := Cons(Car_Obj, Cdr_Obj);
                        return;
                     exception
                        when Ada.Strings.Index_Error =>
                           Get_Line(Str);
                           I := 1;
                     end;
                  end loop;
               end if;
            exception
               when Ada.Strings.Index_Error =>
                  Get_Line(Str);
                  I := 1;
            end;
         end loop;
      end;

      function Delimiter_At_Index(Str : Unbounded_String;
                                  I : Integer) return Boolean is
      begin
         return Is_Delimiter(Element(Str, I));
      exception
         when Ada.Strings.Index_Error =>
            return True;
      end;

      procedure Read_From_Index(Str : in out Unbounded_String;
                                I : in out Integer;
                                Obj : in out Access_Object) is
      begin

         while I <= Length(Str) loop
            begin
               if Is_Space(Element(Str, I)) then
                  -- Continue
                  I := I + 1;

               elsif Element(Str, I) = ';' then
                  -- This is a comment until the end of the line, we can skip
                  --  the rest of this line by just raising
                  --  Ada.Strings.Index_Error, which will make the reader think
                  --  it hit an end-of-line and ask for a new line.
                  raise Ada.Strings.Index_Error;

               elsif Element(Str, I) = '"' then
                  -- Read a String
                  I := I + 1;
                  declare
                     Obj_Str : Unbounded_String;
                  begin
                     Read_String(Str, Obj_Str, I);
                     Obj := Make_String(Obj_Str);
                     return;
                  end;

               elsif Element(Str, I) = ''' then
                  -- Read a quoted expression
                  declare
                     Q_Obj : Access_Object;
                  begin
                     I := I + 1;
                     Read_From_Index(Str, I, Q_Obj);
                     Obj := Cons(Quote_Symbol, Cons(Q_Obj, The_Empty_List));
                     return;
                  end;

               elsif
                 (Is_Initial(Element(Str, I)) and then not Is_Digit(Element(Str, I))) or else
                 ((Element(Str, I) = '+' or else Element(Str, I) = '-') and then
                    Delimiter_At_Index(Str, I + 1))
               then
                  declare
                     Symb_Str : Unbounded_String;
                  begin
                     begin
                        while
                          Is_Initial(Element(Str, I)) or else Is_Digit(Element(Str, I))
                          or else Element(Str, I) = '+' or else Element(Str, I) = '-'
                        loop
                           Append(Symb_Str, Element(Str, I));
                           I := I + 1;
                        end loop;
                     exception
                        when Ada.Strings.Index_Error =>
                           -- End of line, no more chars to add to the symbol
                           --  string.
                           null;
                     end;

                     Obj := Make_Symbol(Symb_Str);
                     return;
                  end;

               -- Lists
               elsif Element(Str, I) = '(' then
                  I := I + 1;
                  Read_Pair(Str, I, Obj);
                  return;

               elsif Element(Str, I) = '#' then
                  I := I + 1;
                  if Element(Str, I) = '\' then
                     -- Read a character
                     I := I + 1;
                     Read_Character(Str, I, Obj);
                     return;

                  else
                     -- Read a boolean
                     case Element(Str, I) is
                        when 't' => Obj := True_Singleton;
                        when 'f' => Obj := False_Singleton;
                        when others =>
                           Stderr("Unknown boolean literal.");
                           raise Constraint_Error;
                     end case;
                     I := I + 1;
                     return;
                  end if;

               elsif Is_Digit(Element(Str, I)) or else Element(Str, I) = '-' then
                  -- Read an integer
                  Read_Integer(Str, I, Obj);
                  return;
               elsif Element(Str, I) = '_' then
                  -- Shortcut for inspecting the env.
                  Obj := Make_Symbol(To_Unbounded_String("_"));
                  return;
               else
                  Stderr("Read illegal state.");
                  raise Constraint_Error;
               end if;
            exception
               when Ada.Strings.Index_Error =>
                  Get_Line(Str);
                  I := 1;
            end;
         end loop;
         return;
      end;

   begin
      Read_From_Index(Str, I, Obj);
   end;

   -- EVAL ----------------------------------------------------------------

   function Eval (Expr : Access_Object;
                  Environ : Access_Object) return Access_Object is

      Exp : Access_Object := Expr;
      Env : Access_Object := Environ;

      function Is_Self_Evaluating (Obj : Access_Object) return Boolean is
      begin
         return Is_Boolean(Obj) or else Is_Integer(Obj)
           or else Is_Character(Obj) or else Is_String(Obj);
      end;

      function Is_Variable (Expr : Access_Object) return Boolean is
      begin
         return Is_Symbol(Expr);
      end;

      function Is_Tagged_List (Obj : Access_Object;
                               Tag : Access_Object) return Boolean is
         The_Car : Access_Object;
      begin
         if (Is_Pair(Obj)) then
            The_Car := Car(Obj);
            return Is_Symbol(The_Car) and then The_Car = Tag;
         end if;
         return False;
      end;

      function Is_Begin (Expr : Access_Object) return Boolean is
      begin
         return Is_Tagged_List(Expr, Begin_Symbol);
      end;

      function Begin_Actions (Expr : Access_Object) return Access_Object is
      begin
         return Cdr(Expr);
      end;

      function Is_Quoted (Obj : Access_Object) return Boolean is
      begin
         return Is_Tagged_List(Obj, Quote_Symbol);
      end;

      function Text_Of_Quotation (Obj : Access_Object) return Access_Object is
      begin
         return Cadr(Obj);
      end;

      function Is_Assignment (Expr : Access_Object) return Boolean is
      begin
         return Is_Tagged_List(Expr, Set_Symbol);
      end;

      function Assignment_Variable (Expr : Access_Object) return Access_Object is
      begin
         return Cadr(Expr);
      end;

      function Assignment_Value (Expr : Access_Object) return Access_Object is
      begin
         return Caddr(Expr);
      end;

      function Is_Definition (Expr : Access_Object) return Boolean is
      begin
         return Is_Tagged_List(Expr, Define_Symbol);
      end;

      function Definition_Variable (Expr : Access_Object) return Access_Object is
         Obj : Access_Object := Cadr(Expr);
      begin
         if Is_Symbol(Obj) then
            return Obj;
         else
            return Car(Obj);
         end if;
      end;

      function Make_Lambda (Parameters : Access_Object;
                            L_Body : Access_Object) return Access_Object;

      function Definition_Value (Expr : Access_Object) return Access_Object is
      begin
         if Is_Symbol(Cadr(Expr)) then
            return Caddr(Expr);
         else
            return Make_Lambda(Cdadr(Expr), Cddr(Expr));
         end if;
      end;

      function Make_Lambda (Parameters : Access_Object;
                            L_Body : Access_Object) return Access_Object is
      begin
         return Cons(Lambda_Symbol, Cons(Parameters, L_Body));
      end;

      function Is_Lambda (Expr : Access_Object) return Boolean is
      begin
         return Is_Tagged_List(Expr, Lambda_Symbol)
           or else Is_Tagged_List(Expr, Lambda_Char_Symbol);
      end;

      function Lambda_Parameters (Expr : Access_Object) return Access_Object is
      begin
         return Cadr(Expr);
      end;

      function Lambda_Body (Expr : Access_Object) return Access_Object is
      begin
         return Cddr(Expr);
      end;

      function Eval_Assignment (Expr : Access_Object;
                                Env : Access_Object) return Access_Object is
      begin
         Set_Variable_Value(Assignment_Variable(Expr),
                            Eval(Assignment_Value(Expr), Env),
                            Env);
         return Ok_Symbol;
      end;

      function Eval_Definition (Expr : Access_Object;
                                Env : Access_Object) return Access_Object is
      begin
         Define_Variable(Definition_Variable(Expr),
                         Eval(Definition_Value(Expr), Env),
                         Env);
         return Ok_Symbol;
      end;

      function Is_If (Expr : Access_Object) return Boolean is
      begin
         return Is_Tagged_List(Expr, If_Symbol);
      end;

      function If_Has_Else (Expr : Access_Object) return Boolean is
      begin
         return Is_Pair(Cdddr(Expr));
      end;

      function Eval_If_Predicate (Expr : Access_Object;
                                  Env : Access_Object) return Access_Object is
      begin
         return Eval(Cadr(Expr), Env);
      end;

      function Eval_If_True (Expr : Access_Object;
                             Env : Access_Object) return Access_Object is
      begin
         return Eval(Caddr(Expr), Env);
      end;

      function Eval_If_Else (Expr : Access_Object;
                             Env : Access_Object) return Access_Object is
      begin
         return Eval(Cadddr(Expr), Env);
      end;

      function Eval_If (Expr : Access_Object;
                        Env : Access_Object) return Access_Object is
      begin
         if not Is_False(Eval_If_Predicate(Expr, Env)) then
            return Eval_If_True(Expr, Env);
         else
            if If_Has_Else(Expr) then
               return Eval_If_Else(Expr, Env);
            else
               return False_Singleton;
            end if;
         end if;
      end;

      function Is_Application (Expr : Access_Object) return Boolean is
      begin
         return Is_Pair(Expr);
      end;

      function Operator (Expr : Access_Object) return Access_Object is
      begin
         return Car(Expr);
      end;

      function Operands (Expr : Access_Object) return Access_Object is
      begin
         return Cdr(Expr);
      end;

      function Is_No_Operands (Ops : Access_Object) return Boolean is
      begin
         return Is_The_Empty_List(Ops);
      end;

      function First_Operand (Ops : Access_Object) return Access_Object is
      begin
         return Car(Ops);
      end;

      function Rest_Operands (Ops : Access_Object) return Access_Object is
      begin
         return Cdr(Ops);
      end;

      function List_Of_Values (Exps : Access_Object;
                               Env : Access_Object) return Access_Object is
      begin
         if (Is_No_Operands(Exps)) then
            return The_Empty_List;
         else
            return Cons(Eval(First_Operand(Exps), Env),
                        List_Of_Values(Rest_Operands(Exps), Env));
         end if;
      end;

      function Is_Last_Exp (Seq : Access_Object) return Boolean is
      begin
         return Is_The_Empty_List(Cdr(Seq));
      end;

      function First_Exp (Seq : Access_Object) return Access_Object is
      begin
         return Car(Seq);
      end;

      function Rest_Exps (Seq : Access_Object) return Access_Object is
      begin
         return Cdr(Seq);
      end;

      function Is_Cond (Expr : Access_Object) return Boolean is
      begin
         return Is_Tagged_List(Expr, Cond_Symbol);
      end;

      function Cond_Clauses (Expr : Access_Object) return Access_Object is
      begin
         return Cdr(Expr);
      end;

      function Cond_Predicate (Clause : Access_Object) return Access_Object is
      begin
         return Car(Clause);
      end;

      function Is_Cond_Else_Clause (Clause : Access_Object) return Boolean is
      begin
         return Cond_Predicate(Clause) = Else_Symbol;
      end;

      function Cond_Actions (Clause : Access_Object) return Access_Object is
      begin
         return Cdr(Clause);
      end;

      function Sequence_To_Expr (Seq : Access_Object) return Access_Object is
      begin
         if Is_The_Empty_List(Seq) then
            return Seq;
         elsif Is_Last_Exp(Seq) then
            return First_Exp(Seq);
         else
            return Make_Begin(Seq);
         end if;
      end;

      function Make_If (Predicate : Access_Object;
                        Consequent : Access_Object;
                        Alternative : Access_Object) return Access_Object is
      begin
         return Cons(If_Symbol,
                     Cons(Predicate,
                          Cons(Consequent,
                               Cons(Alternative,
                                    The_Empty_List))));
      end;

      function Expand_Clauses (Clauses : Access_Object) return Access_Object is
      begin
         if Is_The_Empty_List(Clauses) then
            return False_Singleton; -- No else clause
         else
            declare
               First : Access_Object := Car(Clauses);
               Rest : Access_Object := Cdr(Clauses);
            begin
               if Is_Cond_Else_Clause(First) then
                  if Is_The_Empty_List(Rest) then
                     return Sequence_To_Expr(Cond_Actions(First));
                  else
                     Stderr("'else' is not the last clause in 'cond' expression.");
                     raise Constraint_Error;
                  end if;
               else
                  return Make_If(Cond_Predicate(First),
                                 Sequence_To_Expr(Cond_Actions(First)),
                                 Expand_Clauses(Rest));
               end if;
            end;
         end if;
      end;

      function Cond_To_If (Expr : Access_Object) return Access_Object is
      begin
         return Expand_Clauses(Cond_Clauses(Expr));
      end;

      function Is_Let (Expr : Access_Object) return Boolean is
      begin
         return Is_Tagged_List(Expr, Let_Symbol);
      end;

      function Let_Bindings (Expr : Access_Object) return Access_Object is
      begin
         return Cadr(Expr);
      end;

      function Let_Body (Expr : Access_Object) return Access_Object is
      begin
         return Cddr(Expr);
      end;

      function Binding_Param (Binding : Access_Object) return Access_Object is
      begin
         return Car(Binding);
      end;

      function Binding_Arg (Binding : Access_Object) return Access_Object is
      begin
         return Cadr(Binding);
      end;

      function Binding_Params (Bindings : Access_Object) return Access_Object is
      begin
         if Is_The_Empty_List(Bindings) then
            return The_Empty_List;
         else
            return Cons(Binding_Param(Car(Bindings)),
                        Binding_Params(Cdr(Bindings)));
         end if;
      end;

      function Binding_Args (Bindings : Access_Object) return Access_Object is
      begin
         if Is_The_Empty_List(Bindings) then
            return The_Empty_List;
         else
            return Cons(Binding_Arg(Car(Bindings)),
                        Binding_Args(Cdr(Bindings)));
         end if;
      end;

      function Let_Parameters (Expr : Access_Object) return Access_Object is
      begin
         return Binding_Params(Let_Bindings(Expr));
      end;

      function Let_Arguments (Expr : Access_Object) return Access_Object is
      begin
         return Binding_Args(Let_Bindings(Expr));
      end;

      function Make_Application (Operator : Access_Object;
                                 Operands : Access_Object) return Access_Object is
      begin
         return Cons(Operator, Operands);
      end;

      function Let_To_Application (Expr : Access_Object) return Access_Object is
      begin
         return Make_Application(Make_Lambda(Let_Parameters(Expr),
                                             Let_Body(Expr)),
                                 Let_Arguments(Expr));
      end;

      function Is_And (Expr : Access_Object) return Boolean is
      begin
         return Is_Tagged_List(Expr, And_Symbol);
      end;

      function And_Predicates (Expr : Access_Object) return Access_Object is
      begin
         return Cdr(Expr);
      end;

      function And_Predicates_To_Ifs (Predicates : Access_Object) return Access_Object is
         First : Access_Object := Car(Predicates);
         Rest : Access_Object := Cdr(Predicates);
      begin
         if Is_The_Empty_List(Rest) then
            return First;
         else
            return Make_If(First,
                           And_Predicates_To_Ifs(Rest),
                           False_Singleton);
         end if;
      end;

      function And_To_If (Expr : Access_Object) return Access_Object is
      begin
         return And_Predicates_To_Ifs(And_Predicates(Expr));
      end;

      function Is_Or (Expr : Access_Object) return Boolean is
      begin
         return Is_Tagged_List(Expr, Or_Symbol);
      end;

      function Or_Predicates (Expr : Access_Object) return Access_Object is
      begin
         return Cdr(Expr);
      end;

      function Or_Predicates_To_Ifs (Predicates : Access_Object) return Access_Object is
         First : Access_Object := Car(Predicates);
         Rest : Access_Object := Cdr(Predicates);
      begin
         if Is_The_Empty_List(Rest) then
            return First;
         else
            -- This is a pretty ugly implementation b/c any value that is truthy
            --  will be evaluated twice (ulness it is last in the list and
            --  everything else is false).
            return Make_If(First,
                           First,
                           Or_Predicates_To_Ifs(Rest));
         end if;
      end;

      function Or_To_If (Expr : Access_Object) return Access_Object is
      begin
         return Or_Predicates_To_Ifs(Or_Predicates(Expr));
      end;

      function Is_Apply_Proc (Proc : Access_Object) return Boolean is
      begin
         return Proc.all.Data.Primitive = Apply_Proc'Access;
      end;

      function Is_Eval_Proc (Proc : Access_Object) return Boolean is
      begin
         return Proc.all.Data.Primitive = Eval_Proc'Access;
      end;

      function Eval_Expression (Args : Access_Object) return Access_Object is
      begin
         return Car(Args);
      end;

      function Eval_Environment (Args : Access_Object) return Access_Object is
      begin
         return Cadr(Args);
      end;

   begin
      <<Tailcall>>
      if Is_Self_Evaluating(Exp) then
          return Exp;
      elsif Is_Variable(Exp) then
         return Lookup_Variable_Value(Exp, Env);
      elsif Is_Quoted(Exp) then
         return Text_Of_Quotation(Exp);
      elsif Is_Assignment(Exp) then
         return Eval_Assignment(Exp, Env);
      elsif Is_Definition(Exp) then
         return Eval_Definition(Exp, Env);
      elsif Is_If(Exp) then
         return Eval_If(Exp, Env);
      elsif Is_Cond(Exp) then
         Exp := Cond_To_If(Exp);
         goto Tailcall;
      elsif Is_And(Exp) then
         Exp := And_To_If(Exp);
         goto Tailcall;
      elsif Is_Or(Exp) then
         Exp := Or_To_If(Exp);
         goto Tailcall;
      elsif Is_Lambda(Exp) then
         return Make_Compound_Proc(Lambda_Parameters(Exp),
                                   Lambda_Body(Exp),
                                   env);
      elsif Is_Let(Exp) then
         Exp := Let_To_Application(Exp);
         goto Tailcall;
      elsif Is_Begin(Exp) then
         Exp := Begin_Actions(Exp);
         declare
            Dummy : Access_Object := null;
         begin
            loop
               exit when Is_Last_Exp(Exp);
               Dummy := Eval(First_Exp(Exp), Env);
               Exp := Rest_Exps(Exp);
            end loop;
         end;
         Exp := First_Exp(Exp);
         goto Tailcall;
      elsif Is_Application(Exp) then
         declare
            Proc : Access_Object := Eval(Operator(Exp), Env);
            Args : Access_Object := List_Of_Values(Operands(Exp), Env);
         begin
            if Is_Primitive_Proc(Proc) then
               if Is_Apply_Proc(Proc) then
                  -- Hack to check for the apply procedure (which should never
                  --  actually be called in this hacked up implementation) which
                  --  if we find, we rewrite the AST.
                  Exp := Make_Application(Cadadr(Exp), Car(Cdaddr(Exp)));
                  goto Tailcall;
               elsif Is_Eval_Proc(Proc) then
                  Exp := Eval_Expression(Args);
                  Env := Eval_Environment(Args);
                  goto Tailcall;
               else
                  return Proc.all.Data.Primitive.all(Args);
               end if;
            elsif Is_Compound_Proc(Proc) then
               Env := Extend_Environment(Proc.all.Data.Compound_Proc.Parameters,
                                         Args,
                                         Proc.all.Data.Compound_Proc.Env);
               Exp := Make_Begin(Proc.all.Data.Compound_Proc.L_Body);
               goto Tailcall;
            else
               Stderr("Unknown procedure type");
               raise Constraint_Error;
            end if;
         end;
      else
         Stderr("Cannot eval unknown expression:");
         Print(Exp);
         New_Line;
         raise Constraint_Error;
      end if;
   end;

   -- PRINT ---------------------------------------------------------------

   procedure Print (Obj : in Access_Object) is

      procedure Print_Pair (Pair_Obj : in Access_Object) is
         Car_Obj : Access_Object := Car(Pair_Obj);
         Cdr_Obj : Access_Object := Cdr(Pair_Obj);
      begin
         Print(Car_Obj);
         if Cdr_Obj.all.O_Type = Pair then
            Put(" ");
            Print_Pair(Cdr_Obj);
            return;
         elsif Cdr_Obj = The_Empty_List then
            return;
         else
            Put(" . ");
            Print(Cdr_Obj);
            return;
         end if;
      end;

   begin
      if Obj = null then
         Stderr("Null object type.");
         raise Constraint_Error;
      end if;

      case Obj.all.O_Type is
         when Int =>
            Put(Obj.all.Data.Int, Width => 0);
         when Bool =>
            if Obj.all.Data.Bool then
               Put("#t");
            else
               Put("#f");
            end if;
         when Symbol =>
            Put(Obj.all.Data.Symbol);
         when Char =>
            declare
               Str : Unbounded_String;
            begin
               Append(Str, "#\ ");
               case Obj.all.Data.Char is
                  when ' ' =>
                     Insert(Str, 3, "space");
                  when Character'Val(10) =>
                     Insert(Str, 3, "newline");
                  when others =>
                     Replace_Element(Str, 3, Obj.all.Data.Char);
               end case;
               Put(Str);
            end;
         when Strng =>
            Put('"');
            for I in 1 .. Length(Obj.all.Data.Strng) loop
               if Element(Obj.all.Data.Strng, I) = Character'Val(10) then
                  Put("\n");
               elsif Element(Obj.all.Data.Strng, I) = '"' then
                  Put('\');
                  Put('"');
               elsif Element(Obj.all.Data.Strng, I) = '\' then
                  Put("\\");
               else
                  Put(Element(Obj.all.Data.Strng, I));
               end if;
            end loop;
            Put('"');
         when Empty_List =>
            Put("()");
         when Pair =>
            Put("(");
            Print_Pair(Obj);
            Put(")");
         when Primitive_Proc =>
            Put("#<primitive procedure>");
         when Compound_Proc =>
            Put("#<lambda procedure>");
         when others =>
            Stderr("Cannot write unknown data type.");
            raise Constraint_Error;
      end case;
   end;

   Str : Unbounded_String;
   Obj : Access_Object;

begin

   Init;

   -- REPL ----------------------------------------------------------------

   Ada.Text_Io.Put_Line("Welcome to Bootstrap Scheme -- Ada version.");

   loop
      begin
         Put("> ");
         Get_Line(Str);
         Read(Str, Obj);
         Put("; ");
         Print(Eval(Obj, The_Global_Environment));
         New_Line;
      exception
         when others =>
            Stderr("Restarting REPL");
      end;
   end loop;

end;


-- MUSIC ------------------------------------------------------------------

-- Lifer's Group, Grand Puba, Nightmares On Wax, Binary Star, DJ Shadow, Cut
--  Chemist

