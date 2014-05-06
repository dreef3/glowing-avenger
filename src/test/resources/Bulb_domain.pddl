(define (domain bulb)
	(:predicates
		(L="Лампа горит")
		(B="Лампа исправна")
	)
	(:actions
		(off_light="Выключить выключатель")
		(on_light="Включить выключатель")
	)
	(:kbase (L <-> B & S))
)
