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

	(: action ch_bulb ;Заменить лампу
        :precondition (B? & ~S)
        :effect (B))

	(: action off_cond ;Выключить кондиционер
        :precondition (C?)
        :effect (~C))

	(: action on_cond ;Включить кондиционер
        :precondition (C?)
        :effect (C))

	(: action off_wind ;Закрыть окно
        :precondition (W?)
        :effect (~W))

	(: action on_wind ;Открыть окно
        :precondition (W?)
        :effect (W))

	(:axiom (L <-> B & S))
)

(define (problem switch_on)
	(:domain bulb)
	(:init (~L))
	(:goal (L)))
