(define (domain bulb)
	(:predicates
		(L="Лампа горит")
		(B="Лампа исправна")
		(S="Выключатель включен")
		(C="Кондиционер включен")
		(W="Окно открыто")
	)
	(:actions
		(off_light="Выключить выключатель")
		(on_light="Включить выключатель")
		(ch_bulb="Заменить лампу")
		(off_cond="Выключить кондиционер")
		(on_cond="Включить кондиционер")
		(off_wind="Закрыть окно")
		(on_wind="Открыть окно")
	)
	(:kbase (L <-> B & S))
)

(define (problem switch_on)
	(:domain bulb)
	(:init (~L))
	(:goal (L)))

(: action off_light :effect (~S))

(: action on_light :effect (S & (L | ~L)))

(: action ch_bulb :precond (~S) :effect (B))

(: action off_cond :effect (~C))

(: action on_cond :effect (C))

(: action off_wind :effect (~W))

(: action on_wind :effect (W))

