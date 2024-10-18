# simplebool

A program for parsing boolean expressions and simplifying them.

## TODO

- Implement Eq trait recursively for BoolExpr
- Rules to implement:
'''
Kommutativgesetze 	(1) 	a ∧ b = b ∧ a {\displaystyle a\land b=b\land a} 	(1’) 	a ∨ b = b ∨ a {\displaystyle a\lor b=b\lor a}
Assoziativgesetze 	(2) 	( a ∧ b ) ∧ c = a ∧ ( b ∧ c ) {\displaystyle (a\land b)\land c=a\land (b\land c)} 	(2’) 	( a ∨ b ) ∨ c = a ∨ ( b ∨ c ) {\displaystyle (a\lor b)\lor c=a\lor (b\lor c)}
Distributivgesetze 	(4) 	a ∧ ( b ∨ c ) = ( a ∧ b ) ∨ ( a ∧ c ) {\displaystyle a\land (b\lor c)=(a\land b)\lor (a\land c)} 	(4’) 	a ∨ ( b ∧ c ) = ( a ∨ b ) ∧ ( a ∨ c ) {\displaystyle a\lor (b\land c)=(a\lor b)\land (a\lor c)}
Doppelnegationsgesetz (Involution) 	(7) 	¬ ( ¬ a ) = a {\displaystyle \neg (\neg a)=a} 	
De Morgansche Gesetze 	(8) 	¬ ( a ∧ b ) = ¬ a ∨ ¬ b {\displaystyle \neg (a\land b)=\neg a\lor \neg b} 	(8’) 	¬ ( a ∨ b ) = ¬ a ∧ ¬ b {\displaystyle \neg (a\lor b)=\neg a\land \neg b}
Komplementärgesetze 	(9) 	a ∧ ¬ a = 0 {\displaystyle a\land \neg a=0} 	(9’) 	a ∨ ¬ a = 1 {\displaystyle a\lor \neg a=1}
Absorptionsgesetze 	(11) 	a ∨ ( a ∧ b ) = a {\displaystyle a\lor (a\land b)=a} 	(11’) 	a ∧ ( a ∨ b ) = a {\displaystyle a\land (a\lor b)=a}
'''
