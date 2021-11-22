


(p notice-sign
    =goal>
        isa nback
    =visual-location>
        isa visual-location
        kind speedsign
    ?visual>
        state free
        buffer empty
==>
    =goal>
        isa nback
        status read-nback
    +visual>
        isa move-attention
        screen-pos =visual-location
)

(spp :u 1000)

;-------------------------------------- First two speedsigns -----------------------------------
(p encode-first*
    =goal>
        isa     nback
        num     =placeholder
        = num 1
    =visual>
        isa     speedsign
        value   =slimit
 #|    ?retrieval>
        buffer empty |#
    ?imaginal>
        state  free
==>
    !bind! =cid (get-chunk-id)
    !bind! =num (get-num-sign)
    !bind! =nr (reset-rehearsal) 

    =goal>
        isa     nback
        state   memorize
        num     =num
        2back   =cid
    +imaginal>
        isa     nback-state
        id      =cid
        slimit  =slimit
 )

(p encode-second*
    =goal>
        isa     nback
        num     =placeholder
        = num 2
    =visual>
        isa     speedsign
        value   =slimit
 #|    ?retrieval>
        buffer empty |#
    ?imaginal>
        state  free
==>
    !bind! =cid (get-chunk-id)
    !bind! =num (get-num-sign)
    !bind! =nr (reset-rehearsal) 

    =goal>
        isa     nback
        state   memorize
        num     =num
        1back   =cid
    +imaginal>
        isa     nback-state
        id      =cid
        slimit  =slimit
)


(p add-to-memory*
    =goal>
        isa     nback
        state   memorize
        - target
    =imaginal>
        isa     nback-state
        id      =cid
        slimit  =slimit
==>
    =goal>
        isa     nback
        status  retrieve
        position first
)

;-------------------------------------- general -----------------------------------

(p encode-third
    =goal>
        isa     nback
        num     =placeholder
        = num 3
    =visual>
        isa     speedsign
        value   =slimit
    ?retrieval>
        buffer empty
    ?imaginal>
        state  free
==>
    !bind! =cid (get-chunk-id)
    !bind! =num (get-num-sign)
    !bind! =nr (reset-rehearsal) 

    =goal>
        isa     nback
        state   decide
        num     =num
        target  =cid
        target-slimit   =slimit
        2back   =id
    +imaginal> ;create new chunk for target (0back)
        isa     nback-state
        id      =cid
        slimit  =slimit
    +retrieval> ;retrieve 2back for comparison
        isa     nback-state
        id      =id
)

(p encode-new-sign
    =goal>
        isa     nback
        num     =placeholder
        target  =target
        1back   =1back
        2back   =2back
        > num 3
    =visual 
        isa     speedsign
        value   =slimit
    ?retrieval>
        buffer empty
    ?imaginal>
        state  free
==>
    !bind! =cid (get-chunk-id)
    !bind! =num (get-num-sign)
    !bind! =nr (reset-rehearsal)

    =goal> ;speedlimits are pushed up by one (old 2back is dropped)
        isa     nback
        state   decide
        target  =cid
        target-slimit   =slimit
        1back   =target
        2back   =1back
    +imaginal>
        isa     nback-state
        id      =cid
        slimit  =slimit
    +retrieval>
        isa     nback-state
        id      =1back

 )


(p decide-response-positive
    =goal>
        isa     nback
        state   decide
        target  =cid
        target-slimit   =slimit
    =retrieval>
        isa     nback-state
        slimit  =slimit
==>
    =goal>
        isa     nback
        state   retrieve
        position first
    -imaginal> ;this should already function as add-to-memory

    !output! (Match! =slimit )
)


(p decide-response-negative
    =goal>
        isa     nback
        state   decide
        target  =cid
        target-slimit   =slimit
    =retrieval>
        isa     nback-state
        - slimit  =slimit
==>
    =goal>
        isa     nback
        state   retrieve
        position first
    -imaginal> ;this should already function as add-to-memory

    !output! (No Match!)
)


(p retrieve-1back
    =goal>
        isa     nback
        state   retrieve
        position first
        1back   =id
    +retrieval>
        isa     nback-state
        id      =id 
==>
    =goal>
        isa     nback-state
        state   rehearse
        position second
)

(p retrieve-2back
    =goal>
        isa     nback
        state   retrieve
        position second
        2back   =id
    +retrieval>
        isa     nback-state
        id      =id 
==>
    =goal>
        isa     nback-state
        state   rehearse
        position first
)

(p rehearse-it
    =goal> 
        isa     nback
        state   rehearse
    =retrieval>
==>
    =goal>
        isa     nback
        state   retrieve
)













