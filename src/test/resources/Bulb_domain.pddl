(define (domain bulb)
	(: predicates
		(L) ;Лампа горит
		(B) ;Лампа исправна
		(S) ;Выключатель включен
		(C) ;Кондиционер включен
		(W) ;Окно открыто
	)

	(: action off_light ;Выключить выключатель
		:precondition (S?)
		:effect (~S)
	)

	(: action on_light ;Включить выключатель
        :precondition (S?)
        :effect (S & L?))

	(:axiom (L <-> B & S))
)
