open Value

module Seal = Utils.Seal

let in_channel_key  = Seal.gen_key ()
let out_channel_key = Seal.gen_key ()

let register () =
  (* int functions *)
  DB.register_extern "helium_readInt"
    { mk_value = fun cls -> cls.of_func begin fun v ->
        cls.of_int (read_int ())
    end};
  DB.register_extern "helium_printInt"
    { mk_value = fun cls -> cls.of_func begin fun v ->
        Printf.printf "%d\n" (cls.to_int v);
        cls.v_unit
    end};
  DB.register_extern "helium_addInt"
    { mk_value = fun cls ->
      cls.of_func (fun n1 ->
      cls.of_func (fun n2 ->
        cls.of_int (cls.to_int n1 + cls.to_int n2)))
    };
  DB.register_extern "helium_subInt"
    { mk_value = fun cls ->
      cls.of_func (fun n1 ->
      cls.of_func (fun n2 ->
        cls.of_int (cls.to_int n1 - cls.to_int n2)))
    };
  DB.register_extern "helium_mulInt"
    { mk_value = fun cls ->
      cls.of_func (fun n1 ->
      cls.of_func (fun n2 ->
        cls.of_int (cls.to_int n1 * cls.to_int n2)))
    };
  DB.register_extern "helium_divInt"
    { mk_value = fun cls ->
      cls.of_func (fun n1 ->
      cls.of_func (fun n2 ->
        cls.of_int (cls.to_int n1 / cls.to_int n2)))
    };
  DB.register_extern "helium_modInt"
    { mk_value = fun cls ->
      cls.of_func (fun n1 ->
      cls.of_func (fun n2 ->
        cls.of_int (cls.to_int n1 mod cls.to_int n2)))
    };
  DB.register_extern "helium_eqInt"
    { mk_value = fun cls ->
      cls.of_func (fun n1 ->
      cls.of_func (fun n2 ->
        cls.of_bool (cls.to_int n1 = cls.to_int n2)))
    };
  DB.register_extern "helium_neqInt"
    { mk_value = fun cls ->
      cls.of_func (fun n1 ->
      cls.of_func (fun n2 ->
        cls.of_bool (cls.to_int n1 <> cls.to_int n2)))
    };
  DB.register_extern "helium_ltInt"
    { mk_value = fun cls ->
      cls.of_func (fun n1 ->
      cls.of_func (fun n2 ->
        cls.of_bool (cls.to_int n1 < cls.to_int n2)))
    };
  DB.register_extern "helium_gtInt"
    { mk_value = fun cls ->
      cls.of_func (fun n1 ->
      cls.of_func (fun n2 ->
        cls.of_bool (cls.to_int n1 > cls.to_int n2)))
    };
  DB.register_extern "helium_leInt"
    { mk_value = fun cls ->
      cls.of_func (fun n1 ->
      cls.of_func (fun n2 ->
        cls.of_bool (cls.to_int n1 <= cls.to_int n2)))
    };
  DB.register_extern "helium_geInt"
    { mk_value = fun cls ->
      cls.of_func (fun n1 ->
      cls.of_func (fun n2 ->
        cls.of_bool (cls.to_int n1 >= cls.to_int n2)))
    };
  DB.register_extern "helium_negInt"
    { mk_value = fun cls ->
      cls.of_func (fun n -> cls.of_int (- (cls.to_int n)))
    };
  DB.register_extern "helium_andInt"
    { mk_value = fun cls ->
      cls.of_func (fun n1 ->
      cls.of_func (fun n2 ->
        cls.of_int (cls.to_int n1 land cls.to_int n2)))
    };
  DB.register_extern "helium_orInt"
    { mk_value = fun cls ->
      cls.of_func (fun n1 ->
      cls.of_func (fun n2 ->
        cls.of_int (cls.to_int n1 lor cls.to_int n2)))
    };
  DB.register_extern "helium_xorInt"
    { mk_value = fun cls ->
      cls.of_func (fun n1 ->
      cls.of_func (fun n2 ->
        cls.of_int (cls.to_int n1 lxor cls.to_int n2)))
    };
  DB.register_extern "helium_lslInt"
    { mk_value = fun cls ->
      cls.of_func (fun n1 ->
      cls.of_func (fun n2 ->
        cls.of_int (cls.to_int n1 lsl cls.to_int n2)))
    };
  DB.register_extern "helium_lsrInt"
    { mk_value = fun cls ->
      cls.of_func (fun n1 ->
      cls.of_func (fun n2 ->
        cls.of_int (cls.to_int n1 lsr cls.to_int n2)))
    };
  DB.register_extern "helium_asrInt"
    { mk_value = fun cls ->
      cls.of_func (fun n1 ->
      cls.of_func (fun n2 ->
        cls.of_int (cls.to_int n1 asr cls.to_int n2)))
    };
  DB.register_extern "helium_notInt"
    { mk_value = fun cls ->
      cls.of_func (fun n -> cls.of_int (lnot (cls.to_int n)))
    };
  (* string functions *)
  DB.register_extern "helium_readLine"
    { mk_value = fun cls -> cls.of_func begin fun v ->
        cls.of_string (read_line ())
    end};
  DB.register_extern "helium_printStr"
    { mk_value = fun cls -> cls.of_func begin fun v ->
        Printf.printf "%s" (cls.to_string v);
        cls.v_unit
    end};
  DB.register_extern "helium_appendStr"
    { mk_value = fun cls ->
      cls.of_func (fun s1 ->
      cls.of_func (fun s2 ->
        cls.of_string (cls.to_string s1 ^ cls.to_string s2)))
    };
  DB.register_extern "helium_lengthStr"
    { mk_value = fun cls ->
      cls.of_func (fun s ->
        cls.of_int (String.length (cls.to_string s)))
    };
  DB.register_extern "helium_substringStr"
    { mk_value = fun cls ->
      cls.of_func (fun s ->
      cls.of_func (fun ns ->
      cls.of_func (fun nl ->
        cls.of_string (String.sub (cls.to_string s) (cls.to_int ns) (cls.to_int nl)))))
    };
  DB.register_extern "helium_stringGet"
    { mk_value = fun cls ->
      cls.of_func (fun n ->
      cls.of_func (fun s ->
        cls.of_char (cls.to_string s).[cls.to_int n]))
    };
  DB.register_extern "helium_indexStr"
    { mk_value = fun cls ->
      cls.of_func (fun s ->
      cls.of_func (fun n ->
        cls.of_char (cls.to_string s).[ cls.to_int n]))
    };
  DB.register_extern "helium_string_of_int"
    { mk_value = fun cls ->
      cls.of_func (fun n ->
        cls.of_string (string_of_int (cls.to_int n)))
    };
  DB.register_extern "helium_rawCompareStr"
    { mk_value = fun cls ->
      cls.of_func (fun s1 ->
      cls.of_func (fun s2 ->
        cls.of_int (String.compare (cls.to_string s1) (cls.to_string s2))))
    };
  DB.register_extern "helium_stringRepeat"
    { mk_value = fun cls ->
      cls.of_func (fun n ->
      cls.of_func (fun c ->
        cls.of_string (String.make (cls.to_int n) (cls.to_char c))))
    };
  (* char functions *)
  DB.register_extern "helium_charCode"
    { mk_value = fun cls ->
      cls.of_func (fun c ->
        cls.of_int (Char.code (cls.to_char c)))
    };
  DB.register_extern "helium_charChr"
    { mk_value = fun cls ->
      cls.of_func (fun c ->
        cls.of_char (Char.chr (cls.to_int c)))
    };
  (* sys functions *)
  DB.register_extern "helium_exit"
    { mk_value = fun cls ->
      cls.of_func (fun n ->
        exit (cls.to_int n))
    };
  DB.register_extern "helium_getArgs"
    { mk_value = fun cls ->
      cls.of_func (fun _ ->
        List.fold_right (fun arg args ->
          cls.list_cons (cls.of_string arg) args)
          (Settings.get_args ())
          cls.list_nil)
    };
  (* input functions *)
  DB.register_extern "helium_stdin"
    { mk_value = fun cls ->
      cls.of_seal (Seal.seal in_channel_key stdin)
    };
  DB.register_extern "helium_openIn"
    { mk_value = fun cls ->
      cls.of_func (fun fname ->
        let chan = open_in (cls.to_string fname) in
        cls.of_seal (Seal.seal in_channel_key chan))
    };
  DB.register_extern "helium_closeIn"
    { mk_value = fun cls ->
      cls.of_func (fun ch ->
        let chan = Seal.unseal in_channel_key (cls.to_seal ch) in
        close_in chan;
        cls.v_unit)
    };
  DB.register_extern "helium_input"
    { mk_value = fun cls ->
      cls.of_func (fun ch ->
      cls.of_func (fun n  ->
        let chan = Seal.unseal in_channel_key (cls.to_seal ch) in
        let n    = cls.to_int n in
        let buf  = Bytes.create n in
        let len  = input chan buf 0 n in
        cls.of_string (Bytes.sub_string buf 0 len)))
    };
  (* output functions *)
  DB.register_extern "helium_stdout"
    { mk_value = fun cls ->
      cls.of_seal (Seal.seal out_channel_key stdout)
    };
  DB.register_extern "helium_stderr"
    { mk_value = fun cls ->
      cls.of_seal (Seal.seal out_channel_key stderr)
    };
  DB.register_extern "helium_openOut"
    { mk_value = fun cls ->
      cls.of_func (fun fname ->
        let chan = open_out (cls.to_string fname) in
        cls.of_seal (Seal.seal out_channel_key chan))
    };
  DB.register_extern "helium_closeOut"
    { mk_value = fun cls ->
      cls.of_func (fun ch ->
        let chan = Seal.unseal out_channel_key (cls.to_seal ch) in
        close_out chan;
        cls.v_unit)
    };
  DB.register_extern "helium_outputString"
    { mk_value = fun cls ->
      cls.of_func (fun ch ->
      cls.of_func (fun s ->
        let chan = Seal.unseal out_channel_key (cls.to_seal ch) in
        output_string chan (cls.to_string s);
        cls.v_unit))
    };
  (* other functions *)
  DB.register_extern "helium_assertFalse"
    { mk_value = fun cls ->
      cls.of_func (fun msg ->
        begin
          Printf.eprintf "Assertion failed: %s" (cls.to_string msg);
          exit 4
        end)
    };
  DB.register_extern "helium_exit"
    { mk_value = fun cls ->
      cls.of_func (fun n -> exit (cls.to_int n))
    };

  ()
