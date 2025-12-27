' ============================================================
' Santa Raid (QB64PE) - Scrolling Zigzag Road + PNG Sprites + Snowflakes + Splash + music

Option _Explicit
Randomize Timer

Const SW = 800
Const SH = 600

Const ROAD_W = SW * 0.7
Const ROAD_MARGIN = 40
Const BORDER_THICK = 5

Const MAXN = 9999

' Sprite sizes (your setup)
Const SANTA_W = 50, SANTA_H = 70
Const CAKE_W = 30, CAKE_H = 30
Const KID_W = 50, KID_H = 50
Const TREE_W = 50, TREE_H = 80

' Snowflakes (a few on screen)
Const FLAKE_N = 14
Const FLAKE_SPEED = 1.8

' balls
Const MAXBUL = 9
Const BUL_SPEED = 5.0
Const BUL_R = 6
Const BUL_COOLDOWN = 60


Type Obj
    x As Single
    y As Single
    vx As Single
    kind As Integer ' 1=cake, 2=kid, 3=snowpile
    w As Single
    h As Single
    active As Integer
End Type

Dim Shared speed As Single, baseSpeed As Single
Dim Shared energy As Single, energyMax As Single
Dim Shared gameOver As Integer

Dim Shared santaX As Single, santaY As Single, santaW As Single, santaH As Single

' Road nodes
Dim Shared nCount As Integer
Dim Shared nY(1 To MAXN) As Single
Dim Shared nC(1 To MAXN) As Single

' Zigzag generator
Dim Shared genDir As Integer
Dim Shared genTurnSpeed As Single
Dim Shared genMinC As Single, genMaxC As Single

' Objects
Const MAXOBJ = 80
Dim Shared objs(1 To MAXOBJ) As Obj

' Trees (bottom anchor)
Const MAXTREE = 70
Dim Shared treeX(1 To MAXTREE) As Single
Dim Shared treeY(1 To MAXTREE) As Single
Const TREE_SPACING = 100 ' odleg³oœæ pionowa miêdzy choinkami

' Snowflakes
Dim Shared flakeX(1 To FLAKE_N) As Single
Dim Shared flakeY(1 To FLAKE_N) As Single
Dim Shared flakePhase(1 To FLAKE_N) As Single

' Animation ticker
Dim Shared animTick As Long

' Images
Dim Shared imgSplash As Long
Dim Shared imgSanta(1 To 2) As Long
Dim Shared imgKid(1 To 2) As Long
Dim Shared imgCake As Long
Dim Shared imgTree As Long
Dim Shared imgSnow As Long
Dim Shared snowW As Long, snowH As Long

' game over
Dim Shared imgGameOver As Long
Dim Shared goW As Long, goH As Long

' distance
Dim Shared distancePx As Double
Dim Shared lastKmSpeedup As Integer

' balls
Dim Shared bulX(1 To MAXBUL) As Single
Dim Shared bulY(1 To MAXBUL) As Single
Dim Shared bulActive(1 To MAXBUL) As Integer
Dim Shared shootCD As Integer

' music
Dim Shared sndMusic As Long
Dim Shared musicVol As Single


Screen _NewImage(SW, SH, 32)
_Title "Santa Raid!"
_Limit 60

' load music first
sndMusic = _SndOpen("music.mp3")
If sndMusic > 0 Then
    musicVol = 0.3
    _SndVol sndMusic, musicVol
    _SndLoop sndMusic
End If

LoadSprites

' run splash screen for 5 sec
ShowSplash


InitGame

Do
    HandleInput
    If gameOver = 0 Then UpdateGame
    Render
Loop

' ------------------------------------------------------------
Sub LoadSprites
    imgSplash = _LoadImage("splash.png", 32)

    imgSanta(1) = _LoadImage("santa1.png", 32)
    imgSanta(2) = _LoadImage("santa2.png", 32)

    imgKid(1) = _LoadImage("kid1.png", 32)
    imgKid(2) = _LoadImage("kid2.png", 32)

    imgCake = _LoadImage("cake.png", 32)
    imgTree = _LoadImage("tree.png", 32)
    imgSnow = _LoadImage("snow.png", 32)


    snowW = _Width(imgSnow)
    snowH = _Height(imgSnow)

    imgGameOver = _LoadImage("game_over.png", 32)

    goW = _Width(imgGameOver)
    goH = _Height(imgGameOver)


End Sub

Sub ShowSplash
    Do
        Cls
        _PutImage (0, 0)-(SW, SH), imgSplash
        _Display
        _Limit 60

        If _KeyHit <> 0 Then Exit Do
        If _KeyDown(27) Then System
    Loop
End Sub

Sub InitGame
    Dim i As Integer
    distancePx = 0
    lastKmSpeedup = 0

    baseSpeed = 0.5
    speed = baseSpeed

    energyMax = 100
    energy = energyMax * .85
    gameOver = 0

    santaW = SANTA_W
    santaH = SANTA_H
    santaX = SW / 2
    santaY = SH - 90

    animTick = 0

    ' Road generator bounds
    genMinC = (ROAD_W / 2) + ROAD_MARGIN
    genMaxC = SW - (ROAD_W / 2) - ROAD_MARGIN
    genDir = IIF(Rnd < 0.5, -1, 1)
    genTurnSpeed = 28

    InitRoadNodes

    For i = 1 To MAXOBJ
        objs(i).active = 0
        objs(i).vx = 0
    Next

    Dim startY As Single
    startY = -TREE_H

    Dim j As Integer
    For j = LBound(treeY) To UBound(treeY)
        treeX(j) = TreeSpawnX
        treeY(j) = startY
        startY = startY + TREE_SPACING
    Next

    ' Snowflakes: few on screen
    For i = 1 To FLAKE_N
        flakeX(i) = RndRange(0, SW)
        flakeY(i) = RndRange(0, SH)
        flakePhase(i) = RndRange(0.85, 1.15)
    Next

    Dim b As Integer
    For b = 1 To MAXBUL
        bulActive(b) = 0
    Next
    shootCD = 0

End Sub

Sub ShootBullet (x As Single, y As Single)
    Dim b As Integer
    For b = 1 To MAXBUL
        If bulActive(b) = 0 Then
            bulActive(b) = -1
            bulX(b) = x
            bulY(b) = y - SANTA_H / 2 - 6
            Exit Sub
        End If
    Next
End Sub

Sub InitRoadNodes
    Dim y As Single, c As Single, stepY As Single
    nCount = 0
    y = -450
    c = SW / 2

    Do While y < SH + 350 And nCount < MAXN
        nCount = nCount + 1
        nY(nCount) = y
        nC(nCount) = c

        stepY = RndRange(30, 100)
        y = y + stepY
        c = NextCenter(c)
    Loop
End Sub

Sub HandleInput
    Dim k As Long
    k = _KeyHit

    If _KeyDown(27) Then System
    If k = Asc("p") Or k = Asc("P") Then PauseScreen
    If sndMusic > 0 Then
        If k = 43 Or k = 61 Then ' + albo =
            musicVol = musicVol + 0.05
            If musicVol > 1 Then musicVol = 1
            _SndVol sndMusic, musicVol
        ElseIf k = 45 Then ' -
            musicVol = musicVol - 0.05
            If musicVol < 0 Then musicVol = 0
            _SndVol sndMusic, musicVol
        End If
    End If


End Sub

Sub PauseScreen
    Do
        _Limit 30
        Render
        DrawCenteredText SH / 2, "PAUZA (P = wróæ)", _RGB32(0, 0, 0), _RGB32(255, 255, 255)
        Dim k As Long
        k = _KeyHit
        If k = Asc("p") Or k = Asc("P") Then Exit Do
        If _KeyDown(27) Then System
    Loop
End Sub

' ------------------------------------------------------------
Sub UpdateGame
    Dim steer As Single, turbo As Integer
    Dim scrollY As Single
    Dim i As Integer

    animTick = animTick + 1

    steer = 0
    If _KeyDown(19200) Then steer = steer - 3
    If _KeyDown(19712) Then steer = steer + 3


    If shootCD > 0 Then shootCD = shootCD - 1
    If _KeyDown(32) And shootCD = 0 Then
        ShootBullet santaX, santaY
        SfxShoot
        shootCD = BUL_COOLDOWN
    End If



    If _KeyDown(18432) Then baseSpeed = baseSpeed + 0.05
    If _KeyDown(20480) Then baseSpeed = baseSpeed - 0.05
    If baseSpeed < 1.2 Then baseSpeed = 1.2
    If baseSpeed > 6.0 Then baseSpeed = 6.0

    speed = baseSpeed
    If turbo Then speed = speed + 3.0
    scrollY = speed * 0.6
    distancePx = distancePx + scrollY

    Dim kmNow As Integer
    kmNow = Int(distancePx / 1000#)

    If kmNow > lastKmSpeedup Then

        baseSpeed = baseSpeed + 0.1
        If baseSpeed > 3.0 Then baseSpeed = 3.0
        lastKmSpeedup = kmNow
    End If


    ' Scroll road nodes
    For i = 1 To nCount
        nY(i) = nY(i) + scrollY
    Next
    MaintainRoadNodes

    ' Santa constrained to road at SantaY
    Dim cSanta As Single, leftEdge As Single, rightEdge As Single
    cSanta = RoadCenterAt(santaY)
    leftEdge = cSanta - ROAD_W / 2
    rightEdge = cSanta + ROAD_W / 2

    santaX = santaX + steer
    If santaX < leftEdge + santaW / 2 Then santaX = leftEdge + santaW / 2
    If santaX > rightEdge - santaW / 2 Then santaX = rightEdge - santaW / 2

    ' Energy drain
    energy = energy - (0.015 * speed)
    If turbo Then energy = energy - (0.10 * speed)
    If energy <= 0 Then energy = 0: gameOver = -1

    ' Spawns
    If Rnd < 0.005 Then SpawnCake
    If Rnd < 0.004 Then SpawnKid
    If Rnd < 0.002 Then SpawnSnowpileRare

    ' Update objects
    For i = 1 To MAXOBJ
        If objs(i).active Then
            objs(i).y = objs(i).y + scrollY

            Dim cObj As Single, l2 As Single, r2 As Single
            cObj = RoadCenterAt(objs(i).y)
            l2 = cObj - ROAD_W / 2
            r2 = cObj + ROAD_W / 2

            If objs(i).kind = 2 Then
                objs(i).x = objs(i).x + objs(i).vx
                If objs(i).x < l2 + 18 Then objs(i).x = l2 + 18: objs(i).vx = -objs(i).vx
                If objs(i).x > r2 - 18 Then objs(i).x = r2 - 18: objs(i).vx = -objs(i).vx
            End If

            If objs(i).y > SH + 160 Then objs(i).active = 0

            IF AABB(santaX - santaW / 2, santaY - santaH / 2, santaW, santaH, _
                    objs(i).x - objs(i).w / 2, objs(i).y - objs(i).h / 2, objs(i).w, objs(i).h) THEN

                Select Case objs(i).kind
                    Case 1
                        energy = energy + 25
                        If energy > energyMax Then energy = energyMax
                        objs(i).active = 0
                        SfxCake
                    Case 2
                        SfxHit
                        gameOver = -1
                    Case 3
                        ' Snowpile: energy only
                        energy = energy - 40
                        If energy < 0 Then energy = 0
                        objs(i).active = 0
                        If energy <= 0 Then gameOver = -1
                End Select
            End If
        End If
    Next

    ' Trees scroll (decor)
    Dim j As Integer
    For j = LBound(treeY) To UBound(treeY)
        treeY(j) = treeY(j) + scrollY

        If treeY(j) > SH + TREE_H Then
            treeY(j) = treeY(j) - ((UBound(treeY) - LBound(treeY) + 1) * TREE_SPACING)
            treeX(j) = TreeSpawnX
        End If
    Next

    ' Snowflakes fall (constant speed)
    For i = 1 To FLAKE_N
        flakeY(i) = flakeY(i) + FLAKE_SPEED * flakePhase(i)
        ' tiny drift for life
        flakeX(i) = flakeX(i) + Sin((animTick + i * 31) / 40.0) * 0.18

        If flakeY(i) > SH + snowH Then
            flakeY(i) = RndRange(-200, -snowH)
            flakeX(i) = RndRange(0, SW)
            flakePhase(i) = RndRange(0.85, 1.15)
        End If

        If flakeX(i) < -snowW Then flakeX(i) = SW + snowW
        If flakeX(i) > SW + snowW Then flakeX(i) = -snowW
    Next


    Dim b As Integer, k As Integer
    For b = 1 To MAXBUL
        If bulActive(b) Then
            bulY(b) = bulY(b) - BUL_SPEED

            ' wylot poza ekran
            If bulY(b) < -20 Then
                bulActive(b) = 0
            Else
                ' trafienie w kidsa
                For k = 1 To MAXOBJ
                    If objs(k).active And objs(k).kind = 2 Then
                    IF AABB(bulX(b) - BUL_R, bulY(b) - BUL_R, BUL_R * 2, BUL_R * 2, _
                            objs(k).x - objs(k).w / 2, objs(k).y - objs(k).h / 2, objs(k).w, objs(k).h) THEN
                            objs(k).active = 0
                            bulActive(b) = 0
                            Exit For
                        End If
                    End If
                Next
            End If
        End If
    Next




End Sub

' ------------------------------------------------------------
' Road maintenance & math
Sub MaintainRoadNodes
    Do While nCount > 2 And nY(1) > SH + 380
        RemoveFirstNode
    Loop
    Do While nCount < MAXN And nY(1) > -260
        PrependNewNode
    Loop
    Do While nCount < MAXN And nY(nCount) < SH + 340
        AppendNewNode
    Loop
End Sub

Sub RemoveFirstNode
    Dim i As Integer
    For i = 1 To nCount - 1
        nY(i) = nY(i + 1)
        nC(i) = nC(i + 1)
    Next
    nCount = nCount - 1
End Sub

Sub PrependNewNode
    Dim stepY As Single, newY As Single, newC As Single
    Dim i As Integer
    stepY = RndRange(30, 100)
    newY = nY(1) - stepY
    newC = NextCenter(nC(1))

    If nCount < MAXN Then
        For i = nCount To 1 Step -1
            nY(i + 1) = nY(i)
            nC(i + 1) = nC(i)
        Next
        nCount = nCount + 1
        nY(1) = newY
        nC(1) = newC
    End If
End Sub

Sub AppendNewNode
    Dim stepY As Single, newY As Single, newC As Single
    stepY = RndRange(30, 100)
    newY = nY(nCount) + stepY
    newC = NextCenter(nC(nCount))

    If nCount < MAXN Then
        nCount = nCount + 1
        nY(nCount) = newY
        nC(nCount) = newC
    End If
End Sub

Function NextCenter! (curC As Single)
    Dim nextC As Single
    nextC = curC + genDir * RndRange(genTurnSpeed * 0.55, genTurnSpeed * 1.05)
    If Rnd < 0.40 Then genDir = -genDir
    If nextC < genMinC Then nextC = genMinC: genDir = 1
    If nextC > genMaxC Then nextC = genMaxC: genDir = -1
    NextCenter = nextC
End Function

Function RoadCenterAt! (y As Single)
    Dim i As Integer
    If y <= nY(1) Then RoadCenterAt = nC(1): Exit Function
    If y >= nY(nCount) Then RoadCenterAt = nC(nCount): Exit Function

    For i = 1 To nCount - 1
        If y >= nY(i) And y <= nY(i + 1) Then
            Dim t As Single, dy As Single
            dy = nY(i + 1) - nY(i)
            If dy = 0 Then
                RoadCenterAt = nC(i)
            Else
                t = (y - nY(i)) / dy
                RoadCenterAt = nC(i) + (nC(i + 1) - nC(i)) * t
            End If
            Exit Function
        End If
    Next
    RoadCenterAt = nC(nCount)
End Function

' ------------------------------------------------------------
' Spawning
Sub SpawnCake
    Dim idx As Integer, ySpawn As Single, c As Single
    idx = FindFreeObj
    If idx = 0 Then Exit Sub

    ySpawn = RndRange(-240, -60)
    c = RoadCenterAt(ySpawn)

    objs(idx).active = -1
    objs(idx).kind = 1
    objs(idx).w = CAKE_W
    objs(idx).h = CAKE_H
    objs(idx).vx = 0
    objs(idx).y = ySpawn
    objs(idx).x = RndRange(c - ROAD_W / 2 + 35, c + ROAD_W / 2 - 35)
End Sub

Sub SpawnKid
    Dim idx As Integer, ySpawn As Single, c As Single
    idx = FindFreeObj
    If idx = 0 Then Exit Sub

    ySpawn = RndRange(-300, -90)
    c = RoadCenterAt(ySpawn)

    objs(idx).active = -1
    objs(idx).kind = 2
    objs(idx).w = KID_W
    objs(idx).h = KID_H
    objs(idx).y = ySpawn

    If Rnd < 0.5 Then
        objs(idx).x = RndRange(c - ROAD_W / 2 + 25, c - ROAD_W / 2 + 140)
    Else
        objs(idx).x = RndRange(c + ROAD_W / 2 - 140, c + ROAD_W / 2 - 25)
    End If

    objs(idx).vx = IIF(Rnd < 0.5, RndRange(-1.0, -0.5), RndRange(0.5, 1.0))
End Sub

Sub SpawnSnowpileRare
    Dim idx As Integer, ySpawn As Single, c As Single
    Dim leftEdge As Single, rightEdge As Single, mid As Single, span As Single

    idx = FindFreeObj
    If idx = 0 Then Exit Sub

    ySpawn = RndRange(-380, -140)
    c = RoadCenterAt(ySpawn)

    leftEdge = c - ROAD_W / 2 + 55
    rightEdge = c + ROAD_W / 2 - 55
    mid = (leftEdge + rightEdge) / 2
    span = (rightEdge - leftEdge) / 2

    objs(idx).active = -1
    objs(idx).kind = 3
    objs(idx).w = 70
    objs(idx).h = 40
    objs(idx).vx = 0
    objs(idx).y = ySpawn

    If Rnd < 0.5 Then
        objs(idx).x = RndRange(leftEdge, mid - span * 0.20)
    Else
        objs(idx).x = RndRange(mid + span * 0.20, rightEdge)
    End If
End Sub

Function FindFreeObj% ()
    Dim i As Integer
    For i = 1 To MAXOBJ
        If objs(i).active = 0 Then FindFreeObj = i: Exit Function
    Next
    FindFreeObj = 0
End Function

' ------------------------------------------------------------
' Render
Sub Render
    Dim y As Integer
    Dim c As Single, leftEdge As Single, rightEdge As Single
    Dim i As Integer

    Dim snowCol As _Unsigned Long, roadCol As _Unsigned Long, borderCol As _Unsigned Long, dashCol As _Unsigned Long
    snowCol = _RGB32(255, 255, 255)
    roadCol = _RGB32(60, 60, 60)
    borderCol = _RGB32(210, 210, 210)
    dashCol = _RGB32(250, 250, 0)

    Cls
    Line (0, 0)-(SW, SH), snowCol, BF


    ' Road scanlines
    For y = 0 To SH
        c = RoadCenterAt(y)
        leftEdge = c - ROAD_W / 2
        rightEdge = c + ROAD_W / 2

        Line (leftEdge, y)-(rightEdge, y), roadCol

        Line (leftEdge, y)-(leftEdge + (BORDER_THICK - 1), y), borderCol
        Line (rightEdge - (BORDER_THICK - 1), y)-(rightEdge, y), borderCol

        If ((y + (Timer * 90)) Mod 40) < 18 Then
            Line (c - 2, y)-(c + 2, y), dashCol
        End If
    Next

    ' Trees behind road
    Dim j As Integer
    For j = LBound(treeY) To UBound(treeY)
        DrawTreeSprite treeX(j), treeY(j)
    Next


    Dim b As Integer
    For b = 1 To MAXBUL
        If bulActive(b) Then
            Circle (bulX(b), bulY(b)), BUL_R, _RGB32(255, 255, 255)
            Paint (bulX(b), bulY(b)), _RGB32(255, 255, 255)
        End If
    Next


    ' Objects
    For i = 1 To MAXOBJ
        If objs(i).active Then
            Select Case objs(i).kind
                Case 1: DrawCakeSprite objs(i).x, objs(i).y
                Case 2: DrawKidSprite objs(i).x, objs(i).y
                Case 3: DrawSnowpile objs(i).x, objs(i).y
            End Select
        End If
    Next

    ' Santa
    DrawSantaSprite santaX, santaY

    ' HUD
    DrawEnergyBar

    ' distance
    DrawDistance

    ' Snowflakes on top
    For i = 1 To FLAKE_N
        _PutImage (flakeX(i), flakeY(i))-(flakeX(i) + snowW, flakeY(i) + snowH), imgSnow
    Next



    ' Game over
    If gameOver Then
        Dim xx As Single, yy As Single

        xx = (SW - goW) / 2
        yy = (SH - goH) / 2

        _PutImage (xx, yy)-(xx + goW, yy + goH), imgGameOver

        If _KeyDown(82) Or _KeyDown(114) Then InitGame
        If _KeyDown(82) Or _KeyDown(114) Then InitGame
    End If

    _Display
End Sub

' ------------------------------------------------------------
' Animated sprite drawing (2 frames)
Function TreeSpawnX! ()

    If Rnd < 0.5 Then
        TreeSpawnX = RndRange(0, SW * 0.1)
    Else
        TreeSpawnX = RndRange(SW * 0.9, SW)
    End If
End Function

Function AnimFrame% ()
    AnimFrame = 1 + ((animTick \ 20) Mod 2) ' \8 faster, \14 slower
End Function

Sub DrawSantaSprite (x As Single, y As Single)
    Dim f As Integer
    f = AnimFrame
    _PutImage (x - SANTA_W / 2, y - SANTA_H / 2)-(x + SANTA_W / 2, y + SANTA_H / 2), imgSanta(f)
End Sub

Sub DrawKidSprite (x As Single, y As Single)
    Dim f As Integer
    f = AnimFrame
    _PutImage (x - KID_W / 2, y - KID_H / 2)-(x + KID_W / 2, y + KID_H / 2), imgKid(f)
End Sub

Sub DrawCakeSprite (x As Single, y As Single)
    _PutImage (x - CAKE_W / 2, y - CAKE_H / 2)-(x + CAKE_W / 2, y + CAKE_H / 2), imgCake
End Sub

Sub DrawTreeSprite (x As Single, bottomY As Single)
    _PutImage (x - TREE_W / 2, bottomY - TREE_H)-(x + TREE_W / 2, bottomY), imgTree
End Sub

' ------------------------------------------------------------
' HUD / primitives
Sub DrawEnergyBar
    Dim barW As Single, barH As Single, x0 As Single, y0 As Single
    Dim pct As Single
    x0 = 14: y0 = 14
    barW = 260: barH = 18

    pct = energy / energyMax
    If pct < 0 Then pct = 0
    If pct > 1 Then pct = 1

    Line (x0 - 2, y0 - 2)-(x0 + barW + 2, y0 + barH + 2), _RGB32(0, 0, 0), BF
    Line (x0, y0)-(x0 + barW, y0 + barH), _RGB32(255, 255, 255), BF
    Line (x0, y0)-(x0 + barW * pct, y0 + barH), _RGB32(255, 120, 160), BF
End Sub

Sub DrawDistance
    Dim km As Double
    Dim txt As String
    Dim x As Integer, y As Integer
    Dim tw As Integer, th As Integer

    km = distancePx / 1000#
    txt = "distance = " + LTrim$(Str$(Int(km))) + " km"

    x = 16
    y = 18 + 22

    tw = _PrintWidth(txt)
    th = _FontHeight


    Line (x - 4, y - 3)-(x + tw + 4, y + th + 3), _RGB32(255, 255, 255), BF


    Line (x - 4, y - 3)-(x + tw + 4, y + th + 3), _RGB32(0, 0, 0), B


    Color _RGB32(0, 0, 0), _RGB32(255, 255, 255)
    _PrintString (x, y), txt


    Color _RGB32(0, 0, 0)
End Sub

Sub DrawCenteredText (y As Single, txt As String, shadowCol As _Unsigned Long, col As _Unsigned Long)
    Dim tw As Long
    tw = _PrintWidth(txt)
    Color _RGB32(0, 0, 0), _RGB32(255, 255, 255)
    _PrintString ((SW - tw) / 2 + 2, y + 1), txt
    _PrintString ((SW - tw) / 2, y), txt
    Color _RGB32(0, 0, 0)
End Sub

Sub DrawSnowpile (x As Single, y As Single)

    Circle (x - 18, y + 6), 18, _RGB32(245, 245, 245)
    Paint (x - 18, y + 6), _RGB32(245, 245, 245)
    Circle (x + 4, y + 2), 22, _RGB32(250, 250, 250)
    Paint (x + 4, y + 2), _RGB32(250, 250, 250)
    Circle (x + 26, y + 8), 16, _RGB32(238, 238, 238)
    Paint (x + 26, y + 8), _RGB32(238, 238, 238)
End Sub

' ------------------------------------------------------------
' Helpers
Function RndRange! (a As Single, b As Single)
    RndRange = a + (b - a) * Rnd
End Function

Function AABB% (ax As Single, ay As Single, aw As Single, ah As Single, bx As Single, by As Single, bw As Single, bh As Single)
    If ax < bx + bw And ax + aw > bx And ay < by + bh And ay + ah > by Then
        AABB = -1
    Else
        AABB = 0
    End If
End Function

Function IIF% (cond As Integer, a As Integer, b As Integer)
    If cond Then IIF = a Else IIF = b
End Function

Sub SfxShoot

    Play "T300 O1 L32 E"
End Sub

Sub SfxCake

    Play "T200 O1 L16 E G"
End Sub

Sub SfxHit

    Play "T120 O1 L8 C P C"
End Sub


