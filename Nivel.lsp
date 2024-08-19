(defun c:Nivel ( /

                              ;; functions
                              *error* calculateLevel drawBlock viewMode InsertLevel createLayers 
                            
                              ;; variable
                              uFlag referenceLevelPos referenceLevelVal textHeight tObj SFlag bFlag pFlag DEFAULT_DIVISION commandText levelValue entity entityText i
                            )
  
  (defun *error* (msg)
    (and uFlag (vla-endundomark aDoc))
    (and tObj (not (vlax-erased-p tObj)) (vla-delete tObj))
    
    (or 
      (wcmatch (strcase msg t) "*break,*cancel*,*exit*") 
      (alert (strcat "ERROR: " msg "**"))
    )
  )
  
  (defun createLayers ()
    (entmake
      '(
        (0 . "LAYER") (2 . "EMPRESAOSM_INDICOTAS") (70 . 0) (62 . 11) (6 . "Continuous")
      )
    )
  )
  
  (defun calculateLevel ( curPosition referenceLevelPos referenceLevelVal division / currentLevel)
    (setq currentLevel (+ referenceLevelVal (/ (- (cadr curPosition) (cadr referenceLevelPos)) division)))
  )
  
  (defun drawBlock ( curPosition / triangle-points-left triangle-points-right points-left points-right textPos coorText)
    (setq 
      triangle-points-right
        (list
          (car curPosition)
          (cadr curPosition)
          (caddr curPosition)
          
          (car curPosition)
          (+ (cadr curPosition) (* textHeight 1.86))
          (caddr curPosition)
          
          (+ (car curPosition) (* textHeight 1.06))
          (+ (cadr curPosition) (* textHeight 1.86))
          (caddr curPosition)
          
          (car curPosition)
          (cadr curPosition)
          (caddr curPosition)
      )
          
    triangle-points-left
      (list
        (car curPosition)
        (cadr curPosition)
        (caddr curPosition)

        (car curPosition)
        (+ (cadr curPosition) (* textHeight 1.86))
        (caddr curPosition)
        
        (- (car curPosition) (* textHeight 1.06))
        (+ (cadr curPosition) (* textHeight 1.86))
        (caddr curPosition)
        
        (car curPosition)
        (cadr curPosition)
        (caddr curPosition)
      )
    )

    (setq 
      points-left (vlax-make-safearray vlax-vbDouble '(0 . 11))
      points-right (vlax-make-safearray vlax-vbDouble '(0 . 11))
    )
    
    (vlax-safearray-fill points-left triangle-points-left)
    (vlax-safearray-fill points-right triangle-points-right)
    
    (vla-AddPolyline modelSpace points-left)
    (vla-AddPolyline modelSpace points-right)
    
    (command "_hatch" "_solid" (entlast) "")
    
    (setq coorText
      (list
        (- (car curPosition) (* textHeight 8.14605))
        (+ (cadr curPosition) (* textHeight 2.47))
        (caddr curPosition)
      )
    )
    
    (setq textPos (vlax-3d-point coorText))

    (vla-addline modelSpace
      (vlax-3d-point
        (+ (car curPosition) (* textHeight 1.56964))
        (+ (cadr curPosition) (* textHeight 2.225))
        (caddr curPosition)
      )   
      
      (vlax-3d-point
        (car coorText)
        (+ (cadr curPosition) (* textHeight 2.225))
        (caddr coorText)
      )
    )
    
    textPos
  )
  
  (defun viewMode ( position referenceLevelPos referenceLevelVal tObj division / currentLevel )
    
    (if (and (not (vlax-erased-p tObj)) (= (vlax-get-property tObj 'Visible) :vlax-false))
      (vla-put-visible tObj :vlax-true)
    )
    
    (vla-put-insertionpoint
      tObj
      (vlax-3D-point (trans position 1 0))
    )
    
    (setq currentLevel (calculateLevel position referenceLevelPos referenceLevelVal division))
  
    (vla-put-textstring tObj
      (vl-string-subst "," "." (rtos currentLevel 2 3))
    )

    (vla-update tObj)
  )
  
  (defun insertLevel ( bFlag pFlag data referenceLevelPos referenceLevelVal division / previousStateTobj precisionPos currentLevel textPos)
    (setvar 'cecolor "256") ;; Color - ByLayer
    
    (setq data (trans data 1 0))
    
    (if pFlag
      ;; Caso sim
      (progn
        (setq previousStateTobj (vlax-get-property tObj 'Visible))       
        (vla-put-visible tObj :vlax-false)

        (setq precisionPos (getpoint "Selecione novamente a posicao: "))
        (setq currentLevel (calculateLevel precisionPos referenceLevelPos referenceLevelVal division))
        
        (vla-put-visible tObj previousStateTobj)
      )
      
      ;; Caso não
      (setq currentLevel (calculateLevel (trans data 0 1) referenceLevelPos referenceLevelVal division))
    )
    
    (if bFlag
      (if pFlag
        (setq textPos (drawBlock precisionPos))
        (setq textPos (drawBlock data))
      )

      (if pFlag
        (setq textPos (vlax-3d-point precisionPos))
        (setq textPos (vlax-3d-point data))
      )
    )
    
    (setvar 'cecolor "0") ;; Color - By Block
    (vla-addtext 
      modelSpace 
      (vl-string-subst "," "." (rtos currentLevel 2 3))
      textPos
      textHeight
    )
  )
  
  (setq
    acadObj (vlax-get-acad-object)
    aDoc (vla-get-activedocument acadObj)
    modelSpace (vla-get-modelspace aDoc)
  )
  
  (setq uFlag (not (vla-startundomark aDoc)))
  
  (setq DEFAULT_DIVISION 100)
  
  
  (createLayers)
  (setvar 'clayer "EMPRESAOSM_INDICOTAS") ;; Color - EMPRESAOSM_INDICOTAS
  
  (setq referenceLevelPos (getpoint "Selecione o nivel de referencia: "))
  (setq referenceLevelVal (getreal "Digite o valor do terreno: "))
  (setq textHeight (getreal "Digite o tamanho do texto: "))

  (setq tObj
    (vla-addtext
      modelSpace
      (rtos referenceLevelVal 2 3)
      (vlax-3d-point
        referenceLevelPos
      )
      textHeight
    )
  )
  
  (vla-put-visible tObj :vlax-false)
  
  (setq 
    SFlag nil
    bFlag nil
    pFlag nil
  )
  
  (setq commandText
    "
    S = Show level/Mostra nivel (Em tempo real)\n
    Q = Substitute MTEXT or TEXT string/Substitui texto de MTEXT ou TEXT\n
    C = Cheatsheet/Planilha de codigos\n
    A = Activated Modes/Modos Ativados\n
    B = Block/Bloco\n
    P = Precision/Precisao\n
    Left Click/Botao esquerdo - Insert/Inserir\n
    + / = Increase/Decrease division value / Aumentar/Diminui valor da divisao\n
    ESC = Close/Fechar
    "
  )
  
  (alert 
    commandText
  ) 
  
  (while 
    (progn
      ;; Colete inputs
      (setq
        grdata (grread 't 15 0)
        code (car grdata)
        data (cadr grdata)
      )
      
      (if (and SFlag (listp data))
        (viewMode data referenceLevelPos referenceLevelVal tObj DEFAULT_DIVISION)
      )

      ;; Analisa o codigo e age de acordo
      (cond
        ;; Botão S pressionado
        ((and (= 2 code) (or (= data 115) (= data 83)))
          (setq SFlag (not SFlag))

          (if (not SFlag)
            (progn
              (prompt "Show Mode Desactivated!\n")
              (vla-put-visible tObj :vlax-false)
            )
            (prompt "Show Mode Activated!\n")
          )
         t
        )

        ;; Botao Q pressionado
        ((and (= 2 code) (or (= data 113) (= data 81)))
            (setq levelValue 
                   (calculateLevel 
                     (getpoint "Selecione a posicao do nivel: ")
                     referenceLevelPos
                     referenceLevelVal
                     DEFAULT_DIVISION
                   )
            )
         
          (setq entity 
                 (vlax-ename->vla-object 
                   (car 
                     (entsel "Selecione o objeto: ")
                   )
                 )
          )
         
          (setq entityText (vla-get-textstring entity))
         
         (setq 
           foundNum nil
           i 1
         )
         
          (while (and (<= i (strlen entityText)) (null foundNum))
            (if (wcmatch (substr entityText i 1) "*[0-9]*")
              (setq foundNum t)
              (setq i (1+ i))
            )
          ) 

          (if foundNum
            (vla-put-textstring entity (strcat (substr entityText 1 (- i 1)) (vl-string-subst "," "." (rtos levelValue 2 3))))
            (vla-put-textstring entity (strcat entityText (vl-string-subst "," "." (rtos levelValue 2 3))))
          )
         
          t
        )

        ;; Botao + pressionado
        ((and (= 2 code) (= data 43))
         (setq DEFAULT_DIVISION (* DEFAULT_DIVISION 10))
         (prompt "Divisao: ")
         (princ DEFAULT_DIVISION)
         (princ "\n")
          t
        )
        
        ;; Botao - pressionado
        ((and (= 2 code) (= data 45))
         (setq DEFAULT_DIVISION (/ DEFAULT_DIVISION 10))
         (prompt "Divisao: ")
         (princ DEFAULT_DIVISION)
         (princ "\n")
          t
        )
        
        ;; Botao A pressionado
        ((and (= 2 code) (or (= data 97) (= data 65)))
          (alert
            (eval
              (strcat
                "[S] - Show Mode: "
                
                (if SFlag
                  "Activated\n"
                  "Desactivated\n"
                )
                
                "[B] - Block Mode: "
                
                (if bFlag
                  "Activated\n"
                  "Desactivated\n"
                )
                
                "[P] - Precision Mode: "
                
                (if pFlag
                  "Activated\n"
                  "Desactivated\n"
                )
              )
            )
          )

          t
        )
        
        ;; Botao B ativado
        ((and (= code 2 ) (or (= data 98) (= data 66)))
          (setq bFlag (not bFlag))
          
          (if bFlag
            (prompt "Block Mode Activated!\n")
            (prompt "Block Mode Desactivated!\n")
          )
          
          t
        )
        
        ;; Botao P pressionado
        ((and (= code 2) (or (= data 112) (= data 80)))
          (setq pFlag (not pFlag))
          
          (if pFlag
          (prompt "Precision Mode Activated!\n")
            (prompt "Precision Mode Desactivated!\n")
          )
          
          t
        )
        
        ;; Botão C pressionado
        ((and (= 2 code) (or (= data 99) (= data 67)))
          (alert 
            commandText
          )
          t
        )
        
        ;; Botao esquerda pressionado
        ((and (= code 3) (listp data))
          (insertLevel bFlag pFlag data referenceLevelPos referenceLevelVal DEFAULT_DIVISION)
          t
        )

        ;; Qualquer botão menos o CAPSLOCK
        ((and (= 2 code) (not (= data 20)))
          (alert "Comando nao reconhecido!\nCaso tenha esquecido os comandos, digite C.")
           t
        )
      )

      t
    )
  )
)

(alert "Lisp carregada com sucesso! Digite \"Nivel\" para comecar!Lisp desenvolvida pela OSM Engenharia")