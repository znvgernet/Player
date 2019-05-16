Imports System.ComponentModel
Imports System.Threading
Imports System.Threading.Thread
Imports AxWMPLib

Public Class Form1
    Public medialist() As String
    Public itmindex As Integer = -1
    Public looptype As Integer = 1
    Public playistopmost As Boolean = False
    Public lbl As New Label
    Public lbl_1 As New Label
    Public playlist As String = Application.StartupPath & "\Playlist.ply"
    Public isautoloop As Boolean = True
    Public X, Y As Integer
    Public fulldisplay As Boolean = False
    Public formtominstate As Boolean = False
    Public media_c_volumn As Integer = 100

    '===================================
    Public Const WM_HOTKEY = &H312
    Public Const MOD_ALT = &H1
    Public Const MOD_CONTROL = &H2
    Public Const MOD_SHIFT = &H4
    Public Const GWL_WNDPROC = (-4)
    Public Declare Auto Function RegisterHotKey Lib "user32.dll" Alias _
        "RegisterHotKey" (ByVal hwnd As IntPtr, ByVal id As Integer, ByVal fsModifiers As Integer, ByVal vk As Integer) As Boolean
    Public Declare Auto Function UnRegisterHotKey Lib "user32.dll" Alias _
        "UnregisterHotKey" (ByVal hwnd As IntPtr, ByVal id As Integer) As Boolean
    '===================================

    Private Sub Form1_KeyPress(sender As Object, e As KeyPressEventArgs) Handles Me.KeyPress
        Select Case Asc(e.KeyChar)
            Case 32  '空格暂停播放
                playorpausesong()
            Case 112 'p键显示或隐藏播放进度条
                showorhideprogress()
            Case 109 'm设置或取消媒体静音
                setkl()
        End Select
        Select Case e.KeyChar
            Case "t", "T" '第一首
                tofirstsong()
            Case "n", "N" '下一首
                nextsong()
            Case "f", "F" '上一首
                topresong()
            Case "l", "L" '最后一首
                tolastsong()
            Case "Q", "q" '停止播放
                stopmedia()
        End Select
        'Console.WriteLine(Asc(e.KeyChar))
    End Sub

    Protected Overrides Sub WndProc(ByRef m As Message)
        If m.Msg = WM_HOTKEY Then
            Select Case m.WParam
                Case 0  '是定义热键的第二个参数
                    setkl(1)
                Case 1
                    addvolume()
                Case 2
                    lowvolume()
                Case 3
                    fastgoorback(1)  '后退2秒
                Case 4
                    fastgoorback()   '前进2秒
                Case 5
                    tofirstsong()
                Case 6
                    tolastsong()
                Case 7
                    nextsong()
                Case 8
                    topresong()
                Case 9
                    stopmedia()
                Case 10
                    showorhideprogress()
                Case 11
                    playorpausesong()
            End Select
        End If
        MyBase.WndProc(m)
    End Sub

    Public Sub fastgoorback(Optional flag As Integer = 0)
        Try
            Dim k As Integer = AxWindowsMediaPlayer1.Ctlcontrols.currentPosition
            Dim m As Integer = AxWindowsMediaPlayer1.currentMedia.duration
            Select Case Int(flag)
                Case 0
                    k = k + 1
                Case 1
                    k = k - 1
            End Select
            If k >= m Then
                k = m
            End If
            If k <= 0 Then
                k = 0
            End If
            AxWindowsMediaPlayer1.Ctlcontrols.currentPosition = k
        Catch ex As Exception

        End Try

    End Sub

    Private Sub sethotkey(ByVal flg As String)
        '释放定义的热键
        UnRegisterHotKey(Handle, 0)
        '注册热键ctrl + T
        Dim isResult As Boolean
        isResult = RegisterHotKey(Handle, 0, MOD_CONTROL, Asc("M")) '注册Ctrl+M的组合键，静音
        isResult = RegisterHotKey(Handle, 1, MOD_CONTROL, 38) '注册向上箭头的组合键，声音加大
        isResult = RegisterHotKey(Handle, 2, MOD_CONTROL, 40) '注册向下箭头的组合键，声音变小
        isResult = RegisterHotKey(Handle, 3, MOD_CONTROL, 37) '注册向左箭头的组合键，后退2秒
        isResult = RegisterHotKey(Handle, 4, MOD_CONTROL, 39) '注册向右箭头的组合键，前进2秒
        isResult = RegisterHotKey(Handle, 5, MOD_CONTROL, Asc("T")) '注册Ctrl+T的组合键，播放第一首
        isResult = RegisterHotKey(Handle, 6, MOD_CONTROL, Asc("L")) '注册Ctrl+L的组合键，播放最后一首
        isResult = RegisterHotKey(Handle, 7, MOD_CONTROL, Asc("N")) '注册Ctrl+N的组合键，播放下一首
        isResult = RegisterHotKey(Handle, 8, MOD_CONTROL, Asc("F")) '注册Ctrl+F的组合键，播放上一首
        isResult = RegisterHotKey(Handle, 9, MOD_CONTROL, Asc("Q")) '注册Ctrl+Q的组合键，停止播放
        isResult = RegisterHotKey(Handle, 10, MOD_CONTROL, Asc("P")) '注册Ctrl+P的组合键，显示或隐藏进度条
        isResult = RegisterHotKey(Handle, 11, MOD_CONTROL, 32) '注册Ctrl+空格的组合键，播放或暂停
    End Sub

    Private Sub Form1_Closing(sender As Object, e As CancelEventArgs) Handles Me.Closing
        UnRegisterHotKey(Handle, 0) '静音
        UnRegisterHotKey(Handle, 1) '增加音量
        UnRegisterHotKey(Handle, 2) '减少音量
        UnRegisterHotKey(Handle, 3) '
        UnRegisterHotKey(Handle, 4) '
        UnRegisterHotKey(Handle, 5) '
        UnRegisterHotKey(Handle, 6) '
        UnRegisterHotKey(Handle, 7) '
        UnRegisterHotKey(Handle, 8) '
        UnRegisterHotKey(Handle, 9) '
        UnRegisterHotKey(Handle, 10)
        UnRegisterHotKey(Handle, 11)
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Dim str As String = "E:\movie\舞出我人生4-MP4\舞出我人生4A.mp4"
        Me.BackColor = Color.Black
        PictureBox3.Controls.Add(PictureBox4)
        PictureBox3.Controls.Add(PictureBox2)
        PictureBox2.Dock = DockStyle.Right
        PictureBox2.Show()
        PictureBox4.Show()
        PictureBox4.Location = New System.Drawing.Point(-PictureBox4.Width, 3)
        Panel1.Dock = DockStyle.Bottom
        Panel2.Top = 0
        Panel2.Dock = DockStyle.Bottom
        'Panel2.Height = 20
        lbl_1.BackColor = Color.Transparent
        lbl_1.AutoSize = False
        lbl_1.ForeColor = Color.Gray
        lbl_1.Height = 20
        lbl_1.TextAlign = ContentAlignment.MiddleRight
        lbl_1.Width = 100
        lbl_1.AllowDrop = True
        AddHandler lbl_1.DragEnter, AddressOf LB_DragEnter '委托拖放数据事件
        AddHandler lbl_1.DragDrop, AddressOf LB_DragDrop '委托数据处理事件
        AddHandler lbl_1.MouseDown, AddressOf lbl_MouseDown '委托拖放数据事件
        AddHandler lbl_1.MouseMove, AddressOf lbl_MouseMove '委托数据处理事件

        lbl.BackColor = Color.Transparent
        lbl.AutoSize = False
        lbl.Height = 20
        'lbl.Location = New System.Drawing.Point(5, 5)
        lbl.ForeColor = Color.Gray
        lbl.Font = New System.Drawing.Font(New FontFamily("宋体"), 9, FontStyle.Regular, System.Drawing.GraphicsUnit.Point)
        '"微软雅黑, 10.5pt"
        lbl.TextAlign = ContentAlignment.MiddleLeft
        lbl.Text = "Waiting..."
        lbl.AllowDrop = True
        AddHandler lbl.DragEnter, AddressOf LB_DragEnter '委托拖放数据事件
        AddHandler lbl.DragDrop, AddressOf LB_DragDrop '委托数据处理事件
        AddHandler lbl.MouseDown, AddressOf lbl_MouseDown '委托拖放数据事件
        AddHandler lbl.MouseMove, AddressOf lbl_MouseMove '委托数据处理事件
        Panel2.Controls.Add(lbl)
        Panel2.Controls.Add(lbl_1)
        lbl_1.Dock = DockStyle.Right
        lbl.Dock = DockStyle.Fill
        Panel1.Visible = False

        AxWindowsMediaPlayer1.Dock = DockStyle.Fill
        AxWindowsMediaPlayer1.uiMode = "none"
        'AxWindowsMediaPlayer1.Controls.Add(lbl)
        AxWindowsMediaPlayer1.settings.volume = 100
        setchoosevoice(0)
        setloop(1)
        setformtop(0)
        AxWindowsMediaPlayer1.stretchToFit = True
        AxWindowsMediaPlayer1.ContextMenuStrip = ContextMenuStrip1
        AxWindowsMediaPlayer1.enableContextMenu = False
        AxWindowsMediaPlayer1.settings.volume = media_c_volumn
        'AxWindowsMediaPlayer1.URL = str
        'AxWindowsMediaPlayer1.Ctlcontrols.play()
        AxWindowsMediaPlayer1.Focus()
        If Command() <> "" Then
            loadcommandfile(Command)
        Else
            Dim pls As String = readplaylist()
            Dim mps
            mps = Split(pls, "|")
            ReDim medialist(UBound(mps))
            For n As Integer = 0 To UBound(mps)
                medialist(n) = mps(n)
            Next
            itmindex = 0
            currentsong()
        End If
        sethotkey(0)


    End Sub
    Private Sub lbl_MouseDown(sender As Object, e As MouseEventArgs)
        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                X = e.X : Y = e.Y
            Case Windows.Forms.MouseButtons.Right
                Me.ContextMenuStrip1.Show(e.X + Me.Left + 9, e.Y + Me.Top + 32)
        End Select
    End Sub

    Private Sub lbl_MouseMove(sender As Object, e As MouseEventArgs)
        If X = e.X And Y = e.Y Then Exit Sub
        If e.Button = Windows.Forms.MouseButtons.Left Then
            Me.Left = Me.Left + e.X - X
            Me.Top = Me.Top + e.Y - Y
        End If
    End Sub
    Private Sub LB_DragDrop(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs)
        Dim klist As New ListBox
        klist.Items.Clear()
        For i As Integer = 0 To UBound(medialist)
            klist.Items.Add(medialist(i))
        Next
        Dim newaddcount As Integer = 0
        Dim kl As Integer = klist.Items.Count - 1
        For Each s As String In e.Data.GetData(DataFormats.FileDrop) '循环枚举数据
            '添加到表
            Dim testFile As New System.IO.FileInfo(s)
            Select Case UCase(testFile.Extension)
                Case ".MP3", ".MP4", ".WMV", ".MPG", ".MPEG", ".MOV", ".AVI", ".RM", ".RMVB", ".RMB", ".MKV", ".WAV", ".WMA", ".DAT"
                    If checkrepeat(s) Then
                    Else
                        klist.Items.Add(s)
                        newaddcount = newaddcount + 1
                    End If

            End Select
        Next
        'If newaddcount > 0 Then
        Timer5.Enabled = False
        lbl_1.Text = "新加入 " & newaddcount & " 个项目到播放列表"
        Timer4.Enabled = True
        'End If
        ReDim medialist(klist.Items.Count - 1)
        For j As Integer = 0 To klist.Items.Count - 1
            medialist(j) = klist.Items(j)
        Next
        '将拖入的曲目设置为当前待播放曲目
        itmindex = kl + 1
        currentsong()
        saveplaylisttofile()
    End Sub

    Private Sub LB_DragEnter(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs)
        e.Effect = DragDropEffects.Link '接受拖放数据，启用拖放效果
    End Sub
    Sub loadcommandfile(ByVal str As String)
        Dim mp() = Split(str, Chr(34))
        Dim mps()
        Dim totalstr As String = ""
        'TextBox1.Text = str
        If UBound(mp) > 0 Then
            For i As Integer = 0 To UBound(mp)
                If i Mod 2 = 0 Then
                    mps = Split(mp(i).ToString.Trim, " ")
                    For j As Integer = 0 To UBound(mps)
                        If totalstr = "" Then
                            totalstr = mps(j)
                        Else
                            totalstr = totalstr & "|" & mps(j)
                        End If
                    Next
                Else
                    If totalstr = "" Then
                        totalstr = mp(i)
                    Else
                        totalstr = totalstr & "|" & mp(i)
                    End If
                End If
            Next
        Else
            mps = Split(str.ToString.Trim, " ")
            If UBound(mps) = 0 Then
                If totalstr = "" Then
                    totalstr = mps(0)
                Else
                    totalstr = totalstr & "|" & mps(0)
                End If
            Else
                For ja As Integer = 0 To UBound(mps)
                    If totalstr = "" Then
                        totalstr = mps(ja)
                    Else
                        totalstr = totalstr & "|" & mps(ja)
                    End If
                Next
            End If
        End If

        mps = Split(totalstr, "|")
        totalstr = ""
        For n As Integer = 0 To UBound(mps)
            If mps(n).ToString.Trim <> "" Then
                If totalstr = "" Then
                    totalstr = mps(n)
                Else
                    totalstr = totalstr & "|" & mps(n)
                End If
            End If
        Next
        Dim pls As String = readplaylist()
        If pls <> "" Then
            totalstr = totalstr & "|" & pls
        End If
        mps = Split(totalstr, "|")
        ReDim medialist(UBound(mps))
        For n As Integer = 0 To UBound(mps)
            medialist(n) = mps(n)
        Next
        itmindex = 0
        currentsong()
    End Sub

    Private Sub AxWindowsMediaPlayer1_KeyPressEvent(sender As Object, e As AxWMPLib._WMPOCXEvents_KeyPressEvent) Handles AxWindowsMediaPlayer1.KeyPressEvent
        Select Case e.nKeyAscii
            Case 32  '空格暂停播放
                playorpausesong()
            Case 112 'p键显示或隐藏播放进度条
                showorhideprogress()
            Case 109 'm设置或取消媒体静音
                setkl(1)
        End Select

        Select Case Chr(e.nKeyAscii)
            Case "t", "T" '第一首
                tofirstsong()
            Case "n", "N" '下一首
                nextsong()
            Case "f", "F" '上一首
                topresong()
            Case "l", "L" '最后一首
                tolastsong()
            Case "q", "Q" '停止播放
                stopmedia()
        End Select
    End Sub

    Private Sub AxWindowsMediaPlayer1_MouseDownEvent(sender As Object, e As AxWMPLib._WMPOCXEvents_MouseDownEvent) Handles AxWindowsMediaPlayer1.MouseDownEvent

        If e.nButton = 2 Then
            Me.ContextMenuStrip1.Show(e.fX + Me.Left + 9, e.fY + Me.Top + 32)
        End If
    End Sub



    Private Sub ToolStripMenuItem2_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem2.Click
        OpenFileDialog1.FileName = ""
        Me.OpenFileDialog1.ShowDialog()
        If OpenFileDialog1.FileName <> "" Then
            Dim i As Long
            ReDim medialist(UBound(OpenFileDialog1.FileNames))
            'Me.Text = UBound(OpenFileDialog1.FileNames).ToString
            For i = 0 To UBound(OpenFileDialog1.FileNames)
                If OpenFileDialog1.FileNames(i) <> "" Then
                    medialist(i) = OpenFileDialog1.FileNames(i)
                End If
            Next
            itmindex = 0
            'AxWindowsMediaPlayer1.Ctlcontrols.stop()
            AxWindowsMediaPlayer1.URL = medialist(itmindex)
            AxWindowsMediaPlayer1.Ctlcontrols.play()
            setmute()
            getfilename(medialist(itmindex))
            If playistopmost Then
                Me.TopMost = True
            End If
            saveplaylisttofile()
        End If
    End Sub

    Private Sub getfilename(ByVal files As String)
        Dim sp = Split(files, "\")
        Dim str As String = sp(UBound(sp)).ToString.Trim
        Dim ps = Split(str, ".")
        Dim rts As String = ""
        For i As Integer = 0 To UBound(ps) - 1
            If rts = "" Then
                rts = ps(i)
            Else
                rts = rts & "." & ps(i)
            End If
        Next
        If Trim(rts) & "" = "" Then
            lbl.Text = "就绪..."
        Else
            lbl.Text = rts
        End If

    End Sub

    Private Sub 播放暂停ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 播放暂停ToolStripMenuItem.Click
        playorpausesong()
    End Sub

    Sub playorpausesong()
        Try
            If AxWindowsMediaPlayer1.playState = WMPLib.WMPPlayState.wmppsPaused Then
                AxWindowsMediaPlayer1.Ctlcontrols.play()
            ElseIf AxWindowsMediaPlayer1.playState = WMPLib.WMPPlayState.wmppsStopped Then
                currentsong()
            Else
                AxWindowsMediaPlayer1.Ctlcontrols.pause()
            End If
            isautoloop = True
        Catch ex As Exception
            AxWindowsMediaPlayer1.settings.setMode("", True)
        End Try
    End Sub

    Private Sub 停止ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 停止ToolStripMenuItem.Click
        stopmedia()
    End Sub

    Sub stopmedia()
        Try
            isautoloop = False
            AxWindowsMediaPlayer1.Ctlcontrols.stop()
        Catch ex As Exception

        End Try
    End Sub

    Private Sub ToolStripMenuItem7_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem7.Click
        Try
            AxWindowsMediaPlayer1.fullScreen = True
            'AxWindowsMediaPlayer1.stretchToFit = True
        Catch ex As Exception

        End Try
    End Sub

    Private Sub 第一个ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 第一个ToolStripMenuItem.Click
        tofirstsong()
    End Sub

    Sub tofirstsong()
        Try
            itmindex = 0
            'AxWindowsMediaPlayer1.Ctlcontrols.stop()
            If System.IO.File.Exists(medialist(itmindex)) Then
                AxWindowsMediaPlayer1.URL = medialist(itmindex)
                AxWindowsMediaPlayer1.Ctlcontrols.play()
                setmute()
                getfilename(medialist(itmindex))
                If playistopmost Then
                    Me.TopMost = True
                End If
            Else
                Deleteitemfromarray(itmindex)
                tofirstsong()
            End If
        Catch ex As Exception

        End Try
    End Sub

    Private Sub 上一个节目ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 上一个节目ToolStripMenuItem.Click
        topresong()
    End Sub

    Sub topresong()
        Try
            itmindex = itmindex - 1
            If itmindex < 0 Then
                itmindex = UBound(medialist)
            End If
            If System.IO.File.Exists(medialist(itmindex)) Then
                'AxWindowsMediaPlayer1.Ctlcontrols.stop()
                AxWindowsMediaPlayer1.URL = medialist(itmindex)
                AxWindowsMediaPlayer1.Ctlcontrols.play()
                setmute()
                getfilename(medialist(itmindex))
                If playistopmost Then
                    Me.TopMost = True
                End If
            Else
                Deleteitemfromarray(itmindex)
                topresong()
            End If
        Catch ex As Exception

        End Try
    End Sub

    Private Sub 下一个节目ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 下一个节目ToolStripMenuItem.Click
        Sleep(500)
        nextsong()
    End Sub

    Private Sub Deleteitemfromarray(ByVal itemindex As Integer)
        Dim n_list As New ListBox
        n_list.Items.Clear()
        For i As Integer = 0 To UBound(medialist)
            If i = itemindex Then
            Else
                n_list.Items.Add(medialist(i))
            End If
        Next
        ReDim medialist(n_list.Items.Count - 1)
        For j As Integer = 0 To n_list.Items.Count - 1
            medialist(j) = n_list.Items(j)
        Next
        itmindex = itmindex - 1
        saveplaylisttofile()
    End Sub

    Private Sub nextsong()
        Try
            itmindex = itmindex + 1
            If itmindex > UBound(medialist) Then
                itmindex = 0
            End If
            If System.IO.File.Exists(medialist(itmindex)) Then
                AxWindowsMediaPlayer1.URL = medialist(itmindex)
                AxWindowsMediaPlayer1.Ctlcontrols.play()
                setmute()
                getfilename(medialist(itmindex))
                If playistopmost Then
                    Me.TopMost = True
                End If
            Else
                Deleteitemfromarray(itmindex)
                nextsong()
            End If
        Catch ex As Exception

        End Try
    End Sub
    Private Sub currentsong()
        Try
            AxWindowsMediaPlayer1.URL = medialist(itmindex)
            If System.IO.File.Exists(medialist(itmindex)) Then
                AxWindowsMediaPlayer1.Ctlcontrols.play()
                setmute()
                getfilename(medialist(itmindex))
                If playistopmost Then
                    Me.TopMost = True
                End If
            Else
                Deleteitemfromarray(itmindex)
                currentsong()
            End If

        Catch ex As Exception

        End Try
    End Sub

    Private Sub nextsong1()
        Try
            Randomize()
            Dim kl As Integer
1:
            kl = Int(Rnd() * (UBound(medialist) + 1))
            If kl = itmindex Then
                GoTo 1
            End If
            itmindex = kl
            'Me.Text = kl.ToString
            If itmindex > UBound(medialist) Then
                itmindex = 0
            ElseIf itmindex < 0 Then
                itmindex = UBound(medialist)
            End If
            If System.IO.File.Exists(medialist(itmindex)) Then
                'AxWindowsMediaPlayer1.Ctlcontrols.stop()
                AxWindowsMediaPlayer1.URL = medialist(itmindex)
                AxWindowsMediaPlayer1.Ctlcontrols.play()
                setmute()
                getfilename(medialist(itmindex))
                If playistopmost Then
                    Me.TopMost = True
                End If
            Else
                Deleteitemfromarray(itmindex)
                nextsong1()
            End If

        Catch ex As Exception

        End Try
    End Sub

    Private Sub 最后一个节目ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 最后一个节目ToolStripMenuItem.Click
        tolastsong()
    End Sub

    Sub tolastsong()
        Try
            itmindex = UBound(medialist)
            If System.IO.File.Exists(medialist(itmindex)) Then
                AxWindowsMediaPlayer1.Ctlcontrols.stop()
                AxWindowsMediaPlayer1.URL = medialist(itmindex)
                AxWindowsMediaPlayer1.Ctlcontrols.play()
                setmute()
                getfilename(medialist(itmindex))
                If playistopmost Then
                    Me.TopMost = True
                End If
            Else
                Deleteitemfromarray(itmindex)
                tolastsong()
            End If
        Catch ex As Exception

        End Try
    End Sub

    Private Sub 立体声ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 立体声ToolStripMenuItem.Click
        setchoosevoice(0)
    End Sub

    Private Sub 左声道ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 左声道ToolStripMenuItem.Click
        setchoosevoice(1)
    End Sub

    Private Sub 右声道ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 右声道ToolStripMenuItem.Click
        setchoosevoice(2)
    End Sub

    Private Sub setchoosevoice(ByVal flg As Integer)
        Select Case flg
            Case 0  '立体声
                左声道ToolStripMenuItem.Checked = False
                右声道ToolStripMenuItem.Checked = False
                立体声ToolStripMenuItem.Checked = True
                AxWindowsMediaPlayer1.settings.balance = 0
            Case 1  '左声道
                左声道ToolStripMenuItem.Checked = True
                右声道ToolStripMenuItem.Checked = False
                立体声ToolStripMenuItem.Checked = False
                AxWindowsMediaPlayer1.settings.balance = -100
            Case 2  '右声道
                左声道ToolStripMenuItem.Checked = False
                右声道ToolStripMenuItem.Checked = True
                立体声ToolStripMenuItem.Checked = False
                AxWindowsMediaPlayer1.settings.balance = 100
            Case Else
                左声道ToolStripMenuItem.Checked = False
                右声道ToolStripMenuItem.Checked = False
                立体声ToolStripMenuItem.Checked = True
                AxWindowsMediaPlayer1.settings.balance = 0
        End Select
    End Sub

    Private Sub 媒体静音ToolStripMenuItem_Click(sender As Object, e As EventArgs)
        setkl()
    End Sub

    Sub setkl(Optional mflg As Integer = 0)
        静音MToolStripMenuItem.Checked = Not 静音MToolStripMenuItem.Checked
        If mflg = 1 Then
            setmute(1)
        Else
            setmute()
        End If

    End Sub

    Private Sub setmute(Optional mflag As Integer = 0)
        If 静音MToolStripMenuItem.Checked Then
            AxWindowsMediaPlayer1.settings.mute = True
            If mflag = 1 Then
                Timer5.Enabled = False
                lbl_1.Text = "媒体静音"
                Timer4.Enabled = True
            End If
        Else
            AxWindowsMediaPlayer1.settings.mute = False
            If mflag = 1 Then
                Timer5.Enabled = False
                lbl_1.Text = "取消媒体静音"
                Timer4.Enabled = True
            End If
        End If
    End Sub

    Private Sub setloop(ByVal flg As Integer)
        Select Case flg
            Case 0
                单曲循环ToolStripMenuItem.Checked = True
                有序循环ToolStripMenuItem1.Checked = False
                无序训话ToolStripMenuItem.Checked = False
                looptype = 0
            Case 1
                单曲循环ToolStripMenuItem.Checked = False
                有序循环ToolStripMenuItem1.Checked = True
                无序训话ToolStripMenuItem.Checked = False
                looptype = 1
            Case 2
                单曲循环ToolStripMenuItem.Checked = False
                有序循环ToolStripMenuItem1.Checked = False
                无序训话ToolStripMenuItem.Checked = True
                looptype = 2
        End Select
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        Try
            If UBound(medialist) >= 0 Then
                If isautoloop = True And (AxWindowsMediaPlayer1.playState = 8 Or AxWindowsMediaPlayer1.playState = 12 Or AxWindowsMediaPlayer1.playState = 10 Or AxWindowsMediaPlayer1.playState = 0 Or AxWindowsMediaPlayer1.playState = 7 Or AxWindowsMediaPlayer1.playState = 1) Then
                    Select Case looptype
                        Case 0
                            currentsong()
                        Case 1
                            nextsong()
                        Case 2
                            nextsong1()
                    End Select
                End If
            End If
            If Me.WindowState = FormWindowState.Minimized Then
                formtominstate = True
            Else
                If formtominstate Then
                    setformposition()
                End If
                formtominstate = False
            End If
        Catch ex As Exception

        End Try
    End Sub

    Private Sub Timer2_Tick(sender As Object, e As EventArgs) Handles Timer2.Tick

        Try
            If AxWindowsMediaPlayer1.playState = WMPLib.WMPPlayState.wmppsPlaying Or WMPLib.WMPPlayState.wmppsPaused Then
                If AxWindowsMediaPlayer1.Ctlcontrols.currentPositionString = "" Then
                    If (AxWindowsMediaPlayer1.currentMedia.durationString.ToString.Length) >= 5 Then
                        Me.Text = "00:00 | " & AxWindowsMediaPlayer1.currentMedia.durationString
                        'lbl_1.Text = "00:00 | " & AxWindowsMediaPlayer1.currentMedia.durationString
                    Else
                        Me.Text = "00:00:00 | " & AxWindowsMediaPlayer1.currentMedia.durationString
                        'lbl_1.Text = "00:00:00 | " & AxWindowsMediaPlayer1.currentMedia.durationString
                    End If
                Else
                    Me.Text = AxWindowsMediaPlayer1.Ctlcontrols.currentPositionString & " | " & AxWindowsMediaPlayer1.currentMedia.durationString
                    'lbl_1.Text = AxWindowsMediaPlayer1.Ctlcontrols.currentPositionString & " | " & AxWindowsMediaPlayer1.currentMedia.durationString
                End If
            Else
                Me.Text = ""
                'lbl_1.Text = ""
            End If
        Catch ex As Exception

        End Try
        'Dim ka As New AudioMixerHelper
        'lbl_1.Text = ka.GetVolume()
        'lbl_1.Text = medialist.LongLength
    End Sub

    Private Sub 始终置顶ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 始终置顶ToolStripMenuItem.Click
        setformtop(2)
    End Sub

    Private Sub 播放时置顶ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 播放时置顶ToolStripMenuItem.Click
        setformtop(1)
    End Sub

    Private Sub 从不置顶ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 从不置顶ToolStripMenuItem.Click
        setformtop(0)
    End Sub

    Private Sub setformtop(ByVal flg As Integer)
        Select Case flg
            Case 0
                从不置顶ToolStripMenuItem.Checked = True
                播放时置顶ToolStripMenuItem.Checked = False
                始终置顶ToolStripMenuItem.Checked = False
                Me.TopMost = False
                playistopmost = False
            Case 1
                从不置顶ToolStripMenuItem.Checked = False
                播放时置顶ToolStripMenuItem.Checked = True
                始终置顶ToolStripMenuItem.Checked = False
                playistopmost = True
                If AxWindowsMediaPlayer1.playState = WMPLib.WMPPlayState.wmppsPlaying Then
                    Me.TopMost = True
                Else
                    Me.TopMost = False
                End If
            Case 2
                从不置顶ToolStripMenuItem.Checked = False
                播放时置顶ToolStripMenuItem.Checked = False
                始终置顶ToolStripMenuItem.Checked = True
                Me.TopMost = True
                playistopmost = False
        End Select
    End Sub

    Private Sub AxWindowsMediaPlayer1_PlayStateChange(sender As Object, e As AxWMPLib._WMPOCXEvents_PlayStateChangeEvent) Handles AxWindowsMediaPlayer1.PlayStateChange

        If playistopmost = True And AxWindowsMediaPlayer1.playState = WMPLib.WMPPlayState.wmppsPlaying Then
            Me.TopMost = True
        Else
            Me.TopMost = False
        End If

        'If AxWindowsMediaPlayer1.currentMedia.imageSourceHeight > 0 Then
        '    If AxWindowsMediaPlayer1.playState = WMPLib.WMPPlayState.wmppsPlaying Then
        '        'medialist(itmindex) = ""
        '        If Me.Height <> AxWindowsMediaPlayer1.currentMedia.imageSourceHeight + 39 And Me.Width <> AxWindowsMediaPlayer1.currentMedia.imageSourceWidth + 16 Then
        '            Me.Height = AxWindowsMediaPlayer1.currentMedia.imageSourceHeight + 39
        '            Me.Width = AxWindowsMediaPlayer1.currentMedia.imageSourceWidth + 16
        '            Me.Top = (SystemInformation.PrimaryMonitorMaximizedWindowSize.Height - Me.Height) / 2
        '            Me.Left = (SystemInformation.PrimaryMonitorSize.Width - Me.Width) / 2
        '            If Me.Height >= SystemInformation.PrimaryMonitorMaximizedWindowSize.Height Or Me.Width >= SystemInformation.PrimaryMonitorMaximizedWindowSize.Width Then
        '                Me.Height = SystemInformation.PrimaryMonitorMaximizedWindowSize.Height - 13
        '                Me.Width = SystemInformation.PrimaryMonitorMaximizedWindowSize.Width
        '                Me.Location = New System.Drawing.Point(0, 0)
        '            End If
        '        End If
        '    End If
        '    ToolStripMenuItem7.Enabled = False
        'Else
        '    'medialist(itmindex) = ""
        '    ToolStripMenuItem7.Enabled = False
        '    If Me.Height <> 134 And Me.Width <> 315 Then
        '        Me.Height = 134
        '        Me.Width = 315
        '        Me.Top = (SystemInformation.PrimaryMonitorSize.Height - Me.Height) / 2
        '        Me.Left = (SystemInformation.PrimaryMonitorSize.Width - Me.Width) / 2
        '    End If
        'End If
    End Sub
    Private Sub AxWindowsMediaPlayer1_OpenStateChange(sender As Object, e As AxWMPLib._WMPOCXEvents_OpenStateChangeEvent) Handles AxWindowsMediaPlayer1.OpenStateChange

        'Me.Text = e.newState
        If AxWindowsMediaPlayer1.currentMedia.imageSourceHeight > 0 Then
            If e.newState = 13 Then
                'Debug.Print("playing")
                'AxWindowsMediaPlayer1.Controls.SetChildIndex(lbl, 1)
                If AxWindowsMediaPlayer1.fullScreen = True Then

                Else
                    If (Me.Height <> AxWindowsMediaPlayer1.currentMedia.imageSourceHeight + 39 And Me.Width <> AxWindowsMediaPlayer1.currentMedia.imageSourceWidth + 16) Then
                        Me.Height = AxWindowsMediaPlayer1.currentMedia.imageSourceHeight + 39
                        Me.Width = AxWindowsMediaPlayer1.currentMedia.imageSourceWidth + 16
                        Select Case Me.WindowState
                            Case FormWindowState.Normal, FormWindowState.Maximized, FormWindowState.Minimized
                                Me.Top = (SystemInformation.PrimaryMonitorMaximizedWindowSize.Height - Me.Height) / 2
                                Me.Left = (SystemInformation.PrimaryMonitorSize.Width - Me.Width) / 2
                        End Select
                        If Me.Height >= SystemInformation.PrimaryMonitorMaximizedWindowSize.Height Or Me.Width >= SystemInformation.PrimaryMonitorMaximizedWindowSize.Width Then
                            Me.Height = SystemInformation.PrimaryMonitorMaximizedWindowSize.Height - 13
                            Me.Width = SystemInformation.PrimaryMonitorMaximizedWindowSize.Width
                            Select Case Me.WindowState
                                Case FormWindowState.Normal, FormWindowState.Maximized, FormWindowState.Minimized
                                    Me.Top = 0
                                    Me.Left = 0
                            End Select
                        End If
                    End If
                End If
            End If

        Else

            If (Me.Height <> 134 And Me.Width <> 315) Then
                Me.Height = 134
                Me.Width = 315
                Select Case Me.WindowState
                    Case FormWindowState.Maximized, FormWindowState.Normal
                        Me.Top = (SystemInformation.PrimaryMonitorSize.Height - Me.Height) / 2
                        Me.Left = (SystemInformation.PrimaryMonitorSize.Width - Me.Width) / 2

                End Select
            End If
        End If
    End Sub

    Private Sub Timer3_Tick(sender As Object, e As EventArgs) Handles Timer3.Tick
        Try
            Dim kl As Double = AxWindowsMediaPlayer1.Ctlcontrols.currentPosition
            Dim lk As Double = AxWindowsMediaPlayer1.currentMedia.duration
            Dim pp As Double = kl / lk * PictureBox3.Width
            PictureBox4.Location = New System.Drawing.Point(-PictureBox4.Width + pp, 3)
        Catch ex As Exception

        End Try
    End Sub

    Private Sub PictureBox3_MouseClick(sender As Object, e As MouseEventArgs) Handles PictureBox3.MouseClick
        Dim p As Double = e.X
        Dim l As Double = PictureBox3.Width
        Dim k As Double = (p / l)
        Try
            AxWindowsMediaPlayer1.Ctlcontrols.currentPosition = AxWindowsMediaPlayer1.currentMedia.duration * k
        Catch ex As Exception

        End Try
    End Sub

    Private Sub PictureBox4_MouseClick(sender As Object, e As MouseEventArgs) Handles PictureBox4.MouseClick
        Dim l As Double = PictureBox4.Right
        Dim k As Double = PictureBox4.Right - PictureBox4.Width + e.X
        Dim p As Double = k / l
        Try
            AxWindowsMediaPlayer1.Ctlcontrols.currentPosition = AxWindowsMediaPlayer1.Ctlcontrols.currentPosition * p
        Catch ex As Exception

        End Try
    End Sub



    Sub showorhideprogress()
        显示播放进度控制面板PToolStripMenuItem.Checked = Not 显示播放进度控制面板PToolStripMenuItem.Checked
        Panel1.Visible = 显示播放进度控制面板PToolStripMenuItem.Checked
    End Sub

    Sub saveplaylisttofile()
        Dim FS As New System.IO.FileStream(playlist, IO.FileMode.Create)
        Dim Bw As New System.IO.BinaryWriter(FS, System.Text.Encoding.Unicode)
        Dim str As String = ""
        Try
            For i As Integer = 0 To UBound(medialist)
                If str = "" Then
                    str = (medialist(i).ToString.Trim)
                Else
                    str = str & vbCrLf & (medialist(i).ToString.Trim)
                End If
            Next
        Catch ex As Exception
            str = ""
        End Try

        Bw.Write(str)
        Bw.Close() '记住，操作完成后及时关闭Bw，否则可能破坏文件
        FS.Close()
    End Sub

    Function readplaylist() As String
        Try
            Dim fs As New System.IO.FileStream(playlist, IO.FileMode.Open)
            Dim br As New System.IO.BinaryReader(fs, System.Text.Encoding.Unicode)
            Dim fls() As String
            fls = br.ReadString.Split(vbCrLf)
            Dim str As String = ""
            For i As Integer = 0 To UBound(fls)
                If str = "" Then
                    str = fls(i).ToString.Trim
                Else
                    str = str & "|" & fls(i).ToString.Trim
                End If
            Next
            readplaylist = str
            br.Close()
            fs.Close()
        Catch ex As Exception
            readplaylist = ""
        End Try
    End Function


    Private Sub setformposition()
        Try
            If Me.WindowState = FormWindowState.Normal Then
                Me.Top = (SystemInformation.PrimaryMonitorMaximizedWindowSize.Height - Me.Height) / 2
                Me.Left = (SystemInformation.PrimaryMonitorSize.Width - Me.Width) / 2
            ElseIf Me.WindowState = FormWindowState.Maximized Then
                Me.Top = 0
                Me.Left = 0
            End If
        Catch ex As Exception

        End Try
    End Sub

    Private Sub Timer4_Tick(sender As Object, e As EventArgs) Handles Timer4.Tick
        lbl_1.Text = ""
        Timer4.Enabled = False
        Timer5.Enabled = True
    End Sub

    Private Sub Timer5_Tick(sender As Object, e As EventArgs) Handles Timer5.Tick
        Try
            If AxWindowsMediaPlayer1.playState = WMPLib.WMPPlayState.wmppsPlaying Or WMPLib.WMPPlayState.wmppsPaused Then
                If AxWindowsMediaPlayer1.Ctlcontrols.currentPositionString = "" Then
                    If (AxWindowsMediaPlayer1.currentMedia.durationString.ToString.Length) >= 5 Then
                        lbl_1.Text = "00:00 | " & AxWindowsMediaPlayer1.currentMedia.durationString
                    Else
                        lbl_1.Text = "00:00:00 | " & AxWindowsMediaPlayer1.currentMedia.durationString
                    End If
                Else
                    lbl_1.Text = AxWindowsMediaPlayer1.Ctlcontrols.currentPositionString & " | " & AxWindowsMediaPlayer1.currentMedia.durationString
                End If
            Else
                lbl_1.Text = ""
            End If
        Catch ex As Exception

        End Try
    End Sub

    Private Sub ToolStripMenuItem10_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem10.Click
        ToolStripMenuItem10.Checked = Not ToolStripMenuItem10.Checked
    End Sub

    Private Sub 窗体无框模式ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 窗体无框模式ToolStripMenuItem.Click
        窗体无框模式ToolStripMenuItem.Checked = Not 窗体无框模式ToolStripMenuItem.Checked
        If 窗体无框模式ToolStripMenuItem.Checked Then
            Me.FormBorderStyle = Windows.Forms.FormBorderStyle.None
        Else
            Me.FormBorderStyle = Windows.Forms.FormBorderStyle.Sizable
        End If
    End Sub

    Private Sub 显示播放进度控制面板PToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 显示播放进度控制面板PToolStripMenuItem.Click
        showorhideprogress()
    End Sub

    Private Sub Form1_Closed(sender As Object, e As EventArgs) Handles Me.Closed
        If ToolStripMenuItem10.Checked Then
            Clearplaylisttofile()
        End If
    End Sub

    Sub Clearplaylisttofile()
        Dim FS As New System.IO.FileStream(playlist, IO.FileMode.Create)
        Dim Bw As New System.IO.BinaryWriter(FS, System.Text.Encoding.Unicode)
        Dim str As String = ""
        Bw.Write(str)
        Bw.Close() '记住，操作完成后及时关闭Bw，否则可能破坏文件
        FS.Close()
    End Sub

    Private Sub 静音MToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 静音MToolStripMenuItem.Click
        setkl(1)
    End Sub

    Private Sub 增大ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 增大ToolStripMenuItem.Click
        addvolume()
    End Sub

    Public Sub addvolume()
        media_c_volumn = media_c_volumn + 1
        If media_c_volumn >= 100 Then
            media_c_volumn = 100
        End If
        AxWindowsMediaPlayer1.settings.volume = media_c_volumn
        Timer5.Enabled = False
        lbl_1.Text = "媒体音量 " & media_c_volumn & " "
        Timer4.Enabled = True
    End Sub

    Private Sub 减小ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 减小ToolStripMenuItem.Click
        lowvolume()
    End Sub

    Public Sub lowvolume()
        media_c_volumn = media_c_volumn - 1
        If media_c_volumn <= 0 Then
            media_c_volumn = 0
        End If
        AxWindowsMediaPlayer1.settings.volume = media_c_volumn
        Timer5.Enabled = False
        lbl_1.Text = "媒体音量 " & media_c_volumn & " "
        Timer4.Enabled = True
    End Sub

    Private Sub 最大ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 最大ToolStripMenuItem.Click
        media_c_volumn = 100
        AxWindowsMediaPlayer1.settings.volume = media_c_volumn
        Timer5.Enabled = False
        lbl_1.Text = "媒体音量 " & media_c_volumn & " "
        Timer4.Enabled = True
    End Sub

    Private Sub 居中ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 居中ToolStripMenuItem.Click
        media_c_volumn = 50
        AxWindowsMediaPlayer1.settings.volume = media_c_volumn
        Timer5.Enabled = False
        lbl_1.Text = "媒体音量 " & media_c_volumn & " "
        Timer4.Enabled = True
    End Sub

    Private Sub 最小ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 最小ToolStripMenuItem.Click
        media_c_volumn = 0
        AxWindowsMediaPlayer1.settings.volume = media_c_volumn
        Timer5.Enabled = False
        lbl_1.Text = "媒体音量 " & media_c_volumn & " "
        Timer4.Enabled = True
    End Sub

    Private Sub 单曲循环ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 单曲循环ToolStripMenuItem.Click
        setloop(0)
    End Sub

    Private Sub 有序循环ToolStripMenuItem1_Click(sender As Object, e As EventArgs) Handles 有序循环ToolStripMenuItem1.Click
        setloop(1)
    End Sub

    Private Sub 无序训话ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 无序训话ToolStripMenuItem.Click
        setloop(2)
    End Sub


    'Private Sub Form1_DragEnter(sender As Object, e As DragEventArgs) Handles Me.DragEnter
    '    e.Effect = DragDropEffects.Link '接受拖放数据，启用拖放效果
    'End Sub
    'Private Sub Form1_DragDrop(sender As Object, e As DragEventArgs) Handles Me.DragDrop
    '    Dim klist As New ListBox
    '    klist.Items.Clear()
    '    For i As Integer = 0 To UBound(medialist)
    '        klist.Items.Add(medialist(i))
    '    Next
    '    Dim newaddcount As Integer = 0
    '    Dim kl As Integer = klist.Items.Count - 1
    '    For Each s As String In e.Data.GetData(DataFormats.FileDrop) '循环枚举数据
    '        '添加到表
    '        Dim testFile As New System.IO.FileInfo(s)
    '        Select Case UCase(testFile.Extension)
    '            Case ".MP3", ".MP4", ".WMV", ".MPG", ".MPEG", ".MOV", ".AVI", ".RM", ".RMVB", ".RMB", ".MKV", ".WAV", ".WMA", ".DAT"
    '                If Not checkrepeat(s) Then
    '                Else
    '                    klist.Items.Add(s)
    '                    newaddcount = newaddcount + 1
    '                End If
    '        End Select
    '    Next
    '    'If newaddcount > 0 Then
    '    Timer5.Enabled = False
    '    lbl_1.Text = "新加入 " & newaddcount & " 个项目到播放列表"
    '    Timer4.Enabled = True
    '    'End If
    '    ReDim medialist(klist.Items.Count - 1)
    '    For j As Integer = 0 To klist.Items.Count - 1
    '        medialist(j) = klist.Items(j)
    '    Next
    '    '将拖入的曲目设置为当前待播放曲目
    '    itmindex = kl + 1
    '    currentsong()
    '    saveplaylisttofile()
    'End Sub

    Public Function checkrepeat(ByVal newstr As String) As Boolean   '去除重复函数，返回数组     
        Dim k As Integer = 0
        If medialist.LongLength > 0 Then
            For i As Integer = 0 To medialist.LongLength - 1
                If UCase(newstr) = UCase(medialist(i)) Then
                    k = k + 1
                End If
            Next
            If k > 0 Then
                Return True
            Else
                Return False
            End If
        Else
            Return False
        End If
    End Function


    Private Sub Panel2_MouseDown(sender As Object, e As MouseEventArgs) Handles Panel2.MouseDown
        Select Case e.Button
            Case Windows.Forms.MouseButtons.Right
                Me.ContextMenuStrip1.Show(e.X + Me.Left + 9, e.Y + Me.Top + 32)
        End Select
    End Sub






End Class
