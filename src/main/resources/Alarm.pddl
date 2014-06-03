(define (domain alarm)
    (: predicates
        (Din)
        (DSb)
        (DSon)
        (KCb)       
        (Door)
        (Alarm)
        (Drive)
        (K)
        (Eon)
        (Immo)
    )

    (: action GetIn
        :precondition (Door)
        :effect (Din)
    )
    
    (: action InsertKey 
        :precondition (Din)
        :effect (K)
    )
    
    (: action EngineOn
        :precondition (Din & K)
        :effect (Eon & Alarm?)
    )
    
    (: axiom (Drive <-> ~Alarm & Eon) )
    
    (: axiom ((Alarm & Eon) >-> Immo) )
)

(define (problem drive)
    (:domain alarm)
    (:init (Door & DSb & KCb & Alarm?) )
    (:goal (Drive) )
)
