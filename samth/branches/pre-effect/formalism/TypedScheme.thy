theory TypedScheme
imports Nominal

begin


(* datatype definitions *)
atom_decl name

nominal_datatype ty = Top | Bot | Int | Bool | Pred "ty" |  Arr "ty" "ty" ("_ \<rightarrow> _" [100,100] 100)

nominal_datatype builtin = Add1 | NumberP | BooleanP | Nott

nominal_datatype lam = 
    Var "name"
  | App "lam" "lam"
  | Abs "\<guillemotleft>name\<guillemotright>lam" "ty"
  | Iff "lam" "lam" "lam"
  | Num "nat"
  | Bool "bool"
  | Proc builtin

syntax
  lam_syn :: "name \<Rightarrow> ty \<Rightarrow> lam \<Rightarrow> lam" ("Lam [_:_]._" [100,100,100] 100)

translations
 "Lam[x:t].b"  \<rightleftharpoons> "Abs x b t"

(* capture-avoiding substitution *)
consts subst :: "lam \<Rightarrow> name \<Rightarrow> lam \<Rightarrow> lam" ("_[_::=_]" [100,100,100] 100)

lemma subst[simp]:
  shows "((Var x)[y::=e]) = (if (x=y) then e else (Var x))"
  and   "(App rator rand)[y::=e] = (App (rator[y::=e]) (rand[y::=e]))"
  and   "(Iff tst thn els)[y::=e] = (Iff (tst[y::=e]) (thn[y::=e]) (els[y::=e]))"
  and   "x\<sharp>(y,t)\<Longrightarrow>(Lam [x:ty].t1)[y::=t] = (Lam [x:ty].(t1[y::=t]))"
  and   "[|x ~= y;x \<sharp>t|] ==>(Lam [x:ty].t1)[y::=t] = (Lam [x:ty].(t1[y::=t]))"
  and   "(Num n)[y::=e] = (Num n)"
  and   "(Proc p)[y::=e] = (Proc p)"
  and   "(Bool b)[y::=e] = (Bool b)"
sorry (* Cheat! FIXME *)

consts 
  values :: "lam set"

inductive values
  intros
  abs_value[simp]: "Lam[x:t].b \<in> values"
  num_value[simp]: "Num n \<in> values"
  bool_value[simp]: "Bool b \<in> values"
  proc_value[simp]: "Proc p \<in> values"

consts
  delta :: "builtin \<Rightarrow> lam \<Rightarrow> lam option"

consts
  reduce :: "(lam * lam) set"

syntax
  reduce_syn :: "lam => lam => bool"  ("_ -\<longrightarrow> _" [200,200] 200)

translations
 "redex -\<longrightarrow> res"  \<rightleftharpoons> "(redex,res) \<in> reduce"

consts ctxt :: "(lam \<Rightarrow> lam) set"

inductive ctxt
intros
  C_Hole[simp]: "(%t. t) \<in> ctxt"
  C_App1[simp]: "E \<in> ctxt \<Longrightarrow> (%t. (App (E t) u)) \<in> ctxt"
  C_App2[simp]: "\<lbrakk>v \<in> values; E \<in> ctxt\<rbrakk> \<Longrightarrow> (%t. (App v (E t))) \<in> ctxt"
  C_Iff[simp]:  "E \<in> ctxt \<Longrightarrow> (%t. (Iff (E t) thn els)) \<in> ctxt"

inductive reduce
  intros
e_beta[simp]: "v : values \<Longrightarrow> (App (Lam[x:t].b) v) -\<longrightarrow> (b[x::=v])"
e_if_true[simp]: "Iff (Bool True) e1 e2 -\<longrightarrow> e1"
e_if_false[simp]: "v ~= Bool True \<Longrightarrow> v : values \<Longrightarrow> Iff v e1 e2 -\<longrightarrow> e2"
e_delta[simp]: "\<lbrakk>v : values; delta p v = Some e\<rbrakk> \<Longrightarrow> App (Proc p) v -\<longrightarrow> e"

constdefs
reduce_multi :: "(lam * lam) set"
"reduce_multi == reduce^*"

constdefs
  closed :: "lam \<Rightarrow> bool"
  closed_def_useless:"closed e == (((supp e) :: name set) = {})"

lemma closed_def:
  "(closed e) = (((supp e) :: name set) = {})"
  by (auto simp add: closed_def_useless)

types varEnv = "name \<rightharpoonup> ty"

consts
  typing :: "(varEnv * lam * ty) set"

syntax
  "_typing" :: "[varEnv, lam, ty] \<Rightarrow> bool"  ("_ \<turnstile> _ : _" [200,200] 200)

translations
 "G \<turnstile> e : t"  \<rightleftharpoons> "(G,e,t) \<in> typing"

inductive typing
intros
t_nat[simp] : "G \<turnstile> (Num n) : ty.Int"
t_var[simp] : "G v = Some t \<Longrightarrow> G \<turnstile> (Var v) : t"
t_bool[simp] : "G \<turnstile> (Bool b) : ty.Bool"
t_abs[simp] : "\<lbrakk>G(x\<mapsto>t) \<turnstile> e : u\<rbrakk> \<Longrightarrow> G \<turnstile> (Lam[x:t].e) : (t \<rightarrow> u)"
t_if[simp] : "\<lbrakk>G \<turnstile> tst : t ; G \<turnstile> thn : u ;  G \<turnstile> els : u \<rbrakk> \<Longrightarrow> G \<turnstile> Iff tst thn els : u"
t_pred [simp]: "\<lbrakk>G \<turnstile> p : Pred t ; G(x\<mapsto>t) \<turnstile> thn : u ;  G \<turnstile> els : u \<rbrakk> \<Longrightarrow> G \<turnstile> Iff (App p (Var x)) thn els : u"
t_not [simp]: "G \<turnstile> Proc Nott : (ty.Bool \<rightarrow> ty.Bool)"
t_add1 [simp]: "G \<turnstile> Proc Add1 : (ty.Int \<rightarrow> ty.Int)"
t_nump [simp]: "G \<turnstile> Proc NumberP : (Pred ty.Int)"
t_boolp [simp]: "G \<turnstile> Proc BooleanP : (Pred ty.Bool)"
t_top [simp] : "\<lbrakk>t \<noteq> Top; G \<turnstile> e : t\<rbrakk> \<Longrightarrow> G \<turnstile> e : Top"
t_app [simp]: "\<lbrakk>G \<turnstile> e : t ; G \<turnstile> d : (t \<rightarrow> u)\<rbrakk> \<Longrightarrow> G \<turnstile> (App d e) : u"

lemma "(App (Lam [x:t].(Var x)) (Num 4)) -\<longrightarrow> Num 4"
  proof -
    have "Num 4 : values" by auto
    hence h:"(App (Lam [x:t].(Var x)) (Num 4)) -\<longrightarrow> ((Var x)[x::=(Num 4)])" by (rule e_beta)
    have "((Var x)[x::=(Num 4)]) = Num 4" by auto
    thus ?thesis using h by auto
  qed

lemma "empty \<turnstile> (Lam[x:Top]. (Iff (App (Proc NumberP) (Var x)) (App (Proc Add1) (Var x)) (Num 12))) : (Top \<rightarrow> ty.Int)"
  apply (rule t_abs)
  apply (rule t_pred)
  apply (rule t_nump)
  apply (rule t_app)
  apply auto
  apply (rule t_var)
  apply (auto)
  done

inductive_cases iff_t_cases : "G \<turnstile> Iff tst thn els : t"
thm iff_t_cases

lemma iff_t_lem : 
  assumes a:"G \<turnstile> Iff tst thn els : t"
  shows "G \<turnstile> els : t"
  using a
  apply (rule iff_t_cases)
  apply (auto simp add: lam.inject)
  apply (simp add : lam.inject)
  apply (rule t_pred)
  apply auto
  thm t_pred
  apply (rule t_pred)

lemma decomposition:
  assumes a:"closed e" and b:"empty \<turnstile> e : t"
  shows "(EX E L R. e = E L \<and> E : ctxt \<and> L -\<longrightarrow> R) \<or> e : values"
  using a b
  proof (nominal_induct e rule: lam.induct)
    case (Var v)
    have "\<not> (closed (Var v))" using closed_def[of "Var v"] by (auto simp add: at_supp at_name_inst lam.supp)
    thus ?case using `closed (Var v)` by auto
  next
    case (Num n)
    thus ?case by simp
  next
    case Abs
    thus ?case by simp
  next
    case Bool
    thus ?case by simp
  next
    case Proc
    thus ?case by simp
  next
    case (Iff tst thn els)
    thus ?case
      proof -
	{
	  assume "tst : values"
	  hence g1:"Iff tst thn els -\<longrightarrow> thn \<or> Iff tst thn els -\<longrightarrow> els"
	  proof (nominal_induct tst rule: lam.induct)
	    case (Bool b)
	    thus ?case by (cases b) (auto simp add: lam.inject)
	  qed (auto)
	  let ?E="(%t. t)"
	  let ?L = "Iff tst thn els"
	  have g2:"?E : ctxt" by simp
	  have g3:"(?E (Iff tst thn els)) = (Iff tst thn els)" by simp
	  have g4:"\<exists> R . Iff tst thn els -\<longrightarrow> R" using g1 by auto
	  have "Iff tst thn els = ?E ?L \<and> ?E : ctxt \<and> (\<exists> R . ?L -\<longrightarrow> R)" using g1 g2 g3 g4 by auto
	  hence "EX E . Iff tst thn els = E ?L \<and> E : ctxt \<and> (\<exists> R . ?L -\<longrightarrow> R)" by auto
	  hence "EX E L . Iff tst thn els = E L \<and> E : ctxt \<and> (\<exists> R . L -\<longrightarrow> R)" by auto
	  hence "EX E L R. Iff tst thn els = E L \<and> E : ctxt \<and> (L -\<longrightarrow> R)" by auto
	}
	moreover
	{
	  assume asm:"tst \<notin> values"
	  have cl:"closed tst" using `closed (Iff tst thn els)` by (auto simp add: closed_def lam.supp)
	  hence ih:"\<exists>E t t'. tst = E t \<and> E \<in> ctxt \<and> t -\<longrightarrow> t'" using Iff asm 
	    by simp
	  then obtain Enew tnew t'new where "tst = Enew tnew" and  "Enew \<in> ctxt" and g1:"tnew -\<longrightarrow> t'new" by auto
	  let ?E="(%t . Iff (Enew t) thn els)"
	  have g2:"?E : ctxt" using `Enew : ctxt` by auto
	  have g3:"?E tnew = Iff tst thn els" using `tst = Enew tnew` by auto
	  have " Iff tst thn els = ?E tnew \<and> ?E \<in> ctxt \<and> tnew -\<longrightarrow> t'new" using g1 g2 g3 by auto
	  hence "\<exists>E . Iff tst thn els = E tnew \<and> E \<in> ctxt \<and> tnew -\<longrightarrow> t'new" by auto
	  hence "\<exists>E t . Iff tst thn els = E t \<and> E \<in> ctxt \<and> t -\<longrightarrow> t'new" by auto
	  hence "\<exists>E t t' . Iff tst thn els = E t \<and> E \<in> ctxt \<and> t -\<longrightarrow> t'" by auto
	}
	ultimately show ?thesis by auto
      qed
    next
      case (App rator rand)
      have cl:"closed rator \<and> closed rand" using App by (simp add: closed_def lam.supp)
      hence ih1: "(\<exists>E t t'. rator = E t \<and> E \<in> ctxt \<and> t -\<longrightarrow> t') \<or> rator \<in> values" using App by auto
      hence ih2: "(\<exists>E t t'. rand = E t \<and> E \<in> ctxt \<and> t -\<longrightarrow> t') \<or> rand \<in> values" using App cl by auto
      thus ?case
	proof -
	  {
	    assume "rator : values"
	    have " empty \<turnstile> rator : t \<rightarrow> u" using App by auto
	  

lemma not_value_has_context:
  assumes a:"e \<notin> values"
  shows "EX E t. E : ctxt \<and> E t = e"
  

lemma unique_decomposition: 
  shows "\<lbrakk>E : ctxt; E t = e; E' : ctxt; E' t' = e\<rbrakk> \<Longrightarrow> E = E'"
  proof (nominal_induct e rule: lam.induct)
    case (Var v)
    have f1:"E = (%t. t)" using Var by cases auto
    have f2:"E'= (%t. t)" using `E' : ctxt` Var by cases auto
    from f1 f2 show ?case by simp
  next
    case (Num n)
    have f1:"E = (%t. t)" using Num by cases auto
    have f2:"E'= (%t. t)" using `E' : ctxt` Num by cases auto
    from f1 f2 show ?case by simp
  next
    case (Bool b)
    have f1:"E = (%t. t)" using Bool by cases auto
    have f2:"E'= (%t. t)" using `E' : ctxt` Bool by cases auto
    from f1 f2 show ?case by simp
  next
    case (Proc p)
    have f1:"E = (%t. t)" using Proc by cases auto
    have f2:"E'= (%t. t)" using `E' : ctxt` Proc by cases auto
    from f1 f2 show ?case by simp
  next
    case Abs
    have f1:"E = (%t. t)" using `E : ctxt` Abs by cases auto
    have f2:"E'= (%t. t)" using `E' : ctxt` Abs by cases auto
    from f1 f2 show ?case by simp
  next

    oops
    
    


