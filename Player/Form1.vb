Imports System.ComponentModel
Imports System.Threading
Imports System.Threading.Thread
Imports AxWMPLib
Imports System.IO
Imports System.Text.RegularExpressions


Public Class Form1

    Public lbl As New Label
    Public lbl_1 As New Label
    Public m_d As Boolean = False
    Public isautoloop As Boolean = True
    Public X, Y As Integer
    Public fulldisplay As Boolean = False
    Public formtominstate As Boolean = False
    Public media_c_volumn As Integer = 100
    Public offset_value As Integer = 0
    Public Current_play_name As String = ""
    Public is_show_lrc As Boolean = False
    Public lrc_array(,) As String

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
            Case 115  's显示或隐藏播放列表
                showsonglist()
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
                    fastgoorback(1)  '后退5秒
                Case 4
                    fastgoorback()   '前进5秒
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
                Case 12
                    showsonglist()
                Case 13
                    If form2show = True And Form2.DataGridView1.SelectedRows.Count = 1 Then
                        Form2.moveitemposition(Form2.DataGridView1.CurrentRow.Index, 1)
                    End If
                Case 14
                    If form2show = True And Form2.DataGridView1.SelectedRows.Count = 1 Then
                        Form2.moveitemposition(Form2.DataGridView1.CurrentRow.Index, 2)
                    End If
                Case 15
                    通透模式ToolStripMenuItem.Checked = Not 通透模式ToolStripMenuItem.Checked
                    If 通透模式ToolStripMenuItem.Checked = True Then
                        Me.TransparencyKey = Color.Black
                    Else
                        Me.TransparencyKey = Nothing
                    End If
                Case 16
                    showorhidestatusbar()
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
                    k = k + 5
                Case 1
                    k = k - 5
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
        UnRegisterHotKey(Handle, 12)
        UnRegisterHotKey(Handle, 13)
        UnRegisterHotKey(Handle, 14)
        UnRegisterHotKey(Handle, 15)
        UnRegisterHotKey(Handle, 16)
        '注册热键ctrl + T
        Dim isResult As Boolean
        isResult = RegisterHotKey(Handle, 0, MOD_ALT + MOD_CONTROL, Asc("M")) '注册Ctrl+M的组合键，静音
        isResult = RegisterHotKey(Handle, 1, MOD_ALT + MOD_CONTROL, 38) '注册向上箭头的组合键，声音加大
        isResult = RegisterHotKey(Handle, 2, MOD_ALT + MOD_CONTROL, 40) '注册向下箭头的组合键，声音变小
        isResult = RegisterHotKey(Handle, 3, MOD_ALT + MOD_CONTROL, 37) '注册向左箭头的组合键，后退5秒
        isResult = RegisterHotKey(Handle, 4, MOD_ALT + MOD_CONTROL, 39) '注册向右箭头的组合键，前进5秒
        isResult = RegisterHotKey(Handle, 5, MOD_ALT + MOD_CONTROL, Asc("T")) '注册Ctrl+T的组合键，播放第一首
        isResult = RegisterHotKey(Handle, 6, MOD_ALT + MOD_CONTROL, Asc("L")) '注册Ctrl+L的组合键，播放最后一首
        isResult = RegisterHotKey(Handle, 7, MOD_ALT + MOD_CONTROL, Asc("N")) '注册Ctrl+N的组合键，播放下一首
        isResult = RegisterHotKey(Handle, 8, MOD_ALT + MOD_CONTROL, Asc("F")) '注册Ctrl+F的组合键，播放上一首
        isResult = RegisterHotKey(Handle, 9, MOD_ALT + MOD_CONTROL, Asc("Q")) '注册Ctrl+Q的组合键，停止播放
        isResult = RegisterHotKey(Handle, 10, MOD_ALT + MOD_CONTROL, Asc("P")) '注册Ctrl+P的组合键，显示或隐藏进度条
        isResult = RegisterHotKey(Handle, 11, MOD_ALT + MOD_CONTROL, 32) '注册Ctrl+空格的组合键，播放或暂停
        isResult = RegisterHotKey(Handle, 12, MOD_ALT + MOD_CONTROL, Asc("B"))  '注册Ctrl+S的组合键，显示或隐藏播放列表
        isResult = RegisterHotKey(Handle, 13, MOD_ALT + MOD_CONTROL, Asc("E"))  '向上移动
        isResult = RegisterHotKey(Handle, 14, MOD_ALT + MOD_CONTROL, Asc("D"))  '向下移动
        isResult = RegisterHotKey(Handle, 15, MOD_ALT + MOD_CONTROL, Asc("U"))  '
        isResult = RegisterHotKey(Handle, 16, MOD_ALT + MOD_CONTROL, Asc("I"))  '
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
        UnRegisterHotKey(Handle, 12)
        UnRegisterHotKey(Handle, 13)
        UnRegisterHotKey(Handle, 14)
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Dim str As String = "E:\movie\舞出我人生4-MP4\舞出我人生4A.mp4"

        sethotkey(0)
        'SplitContainer1.Height = 26
        'SplitContainer1.Dock = DockStyle.Bottom
        Panel3.Dock = DockStyle.Bottom
        Panel5.Dock = DockStyle.Top
        Panel2.Height = 25
        Panel1.BackColor = Color.FromArgb(12, 33, 60)
        Panel2.BackColor = Color.FromArgb(12, 33, 60)
        Panel3.Height = Panel2.Height '+ Panel1.Height
        Me.BackColor = Color.FromArgb(12, 33, 60)
        PictureBox3.Controls.Add(PictureBox4)
        PictureBox3.Controls.Add(PictureBox2)
        PictureBox2.Dock = DockStyle.Right
        PictureBox2.Show()
        PictureBox4.Show()
        PictureBox4.Location = New System.Drawing.Point(-PictureBox4.Width, 3)
        Panel1.Dock = DockStyle.Top
        Panel2.Top = 0
        Panel2.Dock = DockStyle.Bottom
        'Panel2.Height = 20
        lbl_1.BackColor = Color.Transparent
        lbl_1.Cursor = System.Windows.Forms.Cursors.SizeAll
        lbl_1.AutoSize = False
        lbl_1.ForeColor = Color.White
        lbl_1.Height = 20
        lbl_1.TextAlign = ContentAlignment.MiddleRight
        lbl_1.Width = 100

        lbl_1.AllowDrop = True
        AddHandler lbl_1.DragEnter, AddressOf LB_DragEnter '委托拖放数据事件
        AddHandler lbl_1.DragDrop, AddressOf LB_DragDrop '委托数据处理事件
        AddHandler lbl_1.MouseDown, AddressOf lbl_MouseDown '委托拖放数据事件
        AddHandler lbl_1.MouseMove, AddressOf lbl_MouseMove '委托数据处理事件
        'AddHandler lbl_1.MouseMove, AddressOf lbl_MouseLeave
        'AddHandler lbl_1.MouseMove, AddressOf lbl_MouseClick
        'AddHandler lbl_1.MouseMove, AddressOf lbl_MouseEnter
        lbl.BackColor = Color.Transparent
        lbl.AutoSize = False

        lbl.Height = 12
        Dim old As Padding = lbl.Margin
        lbl.Margin = New Padding(old.Left, old.Top, old.Right, 5)
        'lbl.Location = New System.Drawing.Point(5, 5)
        lbl.ForeColor = Color.White
        lbl.Cursor = System.Windows.Forms.Cursors.SizeAll
        lbl.Font = New System.Drawing.Font(New FontFamily("宋体"), 9, FontStyle.Regular, System.Drawing.GraphicsUnit.Point)
        '"微软雅黑, 10.5pt"
        lbl.TextAlign = ContentAlignment.MiddleLeft

        lbl.Text = "Waiting..."
        lbl.AllowDrop = True
        AddHandler lbl.DragEnter, AddressOf LB_DragEnter '委托拖放数据事件
        AddHandler lbl.DragDrop, AddressOf LB_DragDrop '委托数据处理事件
        AddHandler lbl.MouseDown, AddressOf lbl_MouseDown '委托拖放数据事件
        AddHandler lbl.MouseMove, AddressOf lbl_MouseMove '委托数据处理事件
        'AddHandler lbl.MouseMove, AddressOf lbl_MouseLeave
        'AddHandler lbl.MouseMove, AddressOf lbl_MouseClick
        'AddHandler lbl.MouseMove, AddressOf lbl_MouseEnter
        Panel6.Dock = DockStyle.Fill
        Panel6.Controls.Add(lbl)
        Panel2.Controls.Add(lbl_1)
        lbl_1.Dock = DockStyle.Right
        lbl.Dock = DockStyle.Top
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
        Panel4.Dock = DockStyle.Fill
        If Command() <> "" Then
            loadcommandfile(Command)
        Else
            Dim pls As String = readplaylist()
            If pls <> "" Then
                Dim mps
                mps = Split(pls, "|")
                ReDim medialist(UBound(mps) - 1)
                For n As Integer = 0 To UBound(mps) - 1
                    medialist(n) = mps(n)
                Next
                Dim ol = Split(mps(UBound(mps)), ":")
                itmindex = ol(0)
                currentsong(ol(1))
            End If
        End If
    End Sub

    Private Sub lbl_MouseDown(sender As Object, e As MouseEventArgs)
        m_d = True
        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                X = e.X : Y = e.Y
            Case Windows.Forms.MouseButtons.Right
                'Me.ContextMenuStrip1.Show(e.X + Me.Left + 9, e.Y + Me.Top + 32)
                Me.ContextMenuStrip1.Show(MousePosition.X, MousePosition.Y)
        End Select
    End Sub

    Private Sub lbl_MouseEnter(sender As Object, e As MouseEventArgs)
        If e.Button = Windows.Forms.MouseButtons.Left Then
            m_d = False
        End If

    End Sub

    Private Sub lbl_MouseLeave(sender As Object, e As EventArgs)
        m_d = False
    End Sub

    Private Sub lbl_MouseClick(sender As Object, e As MouseEventArgs)
        m_d = True
    End Sub

    Private Sub lbl_MouseMove(sender As Object, e As MouseEventArgs)

        If X = e.X And Y = e.Y Then Exit Sub
        If e.Button = Windows.Forms.MouseButtons.Left Then  'And m_d = True
            Me.Left = Me.Left + e.X - X
            Me.Top = Me.Top + e.Y - Y
            setform2position()
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
                Case ".MP3", ".M4A", ".MP4", ".WMV", ".MPG", ".MPEG", ".MOV", ".AVI", ".RM", ".RMVB", ".RMB", ".MKV", ".WAV", ".WMA", ".DAT"
                    If checkrepeat(s) Then
                    Else
                        klist.Items.Add(s)
                        newaddcount = newaddcount + 1
                    End If
            End Select
        Next
        'If newaddcount > 0 Then
        Timer5.Enabled = False
        Timer4.Enabled = False
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
        If form2show = True Then
            Form2.getplaylist()
        End If
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
            Case "s", "S" '
                showsonglist()
        End Select
    End Sub

    Private Sub AxWindowsMediaPlayer1_MouseDownEvent(sender As Object, e As AxWMPLib._WMPOCXEvents_MouseDownEvent) Handles AxWindowsMediaPlayer1.MouseDownEvent

        If e.nButton = 2 Then
            Me.ContextMenuStrip1.Show(MousePosition.X, MousePosition.Y)
        Else

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
            If form2show Then
                Form2.getplaylist()
            End If
        End If
    End Sub

    Public Sub getfilename(ByVal files As String)
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
            Current_play_name = ""
        Else
            lbl.Text = rts
            Current_play_name = rts
        End If
        Show_lrc()
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
            If 停止循环ToolStripMenuItem.Checked = True Then
                isautoloop = False
            Else
                isautoloop = True
            End If
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
            Dim olditm As Integer = itmindex
            itmindex = 0
            setcurrentrow(itmindex, olditm)
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
            Dim olditm As Integer = itmindex
            Dim newitm As Integer = -1
            itmindex = itmindex - 1
            If itmindex < 0 Then
                itmindex = UBound(medialist)
            End If
            setcurrentrow(itmindex, olditm)
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

    Public Sub nextsong(Optional songindx As Integer = -1)
        Try
            Dim olditm As Integer = itmindex
            Dim newitm As Integer = -1
            If songindx = -1 Then
                itmindex = itmindex + 1
            Else
                itmindex = songindx
            End If
            If itmindex > UBound(medialist) Then
                itmindex = 0
            End If
            setcurrentrow(itmindex, olditm)
            If System.IO.File.Exists(medialist(itmindex)) Then
                AxWindowsMediaPlayer1.URL = medialist(itmindex)
                AxWindowsMediaPlayer1.Ctlcontrols.play()
                If 停止循环ToolStripMenuItem.Checked = True Then
                    isautoloop = False
                Else
                    isautoloop = True
                End If
                setmute()
                getfilename(medialist(itmindex))
                If playistopmost Then
                    Me.TopMost = True
                End If
            Else
                If itmindex = UBound(medialist) Then
                    newitm = 0
                Else
                    newitm = itmindex
                End If
                Deleteitemfromarray(itmindex)
                nextsong(newitm)
            End If
        Catch ex As Exception

        End Try
    End Sub

    Private Sub currentsong(Optional cp As Integer = 0, Optional newit As Integer = -1)
        Try
            AxWindowsMediaPlayer1.Ctlcontrols.stop()
            AxWindowsMediaPlayer1.URL = medialist(itmindex)
            Dim newitm As Integer = -1
            If System.IO.File.Exists(medialist(itmindex)) Then
                AxWindowsMediaPlayer1.Ctlcontrols.currentPosition = cp
                AxWindowsMediaPlayer1.Ctlcontrols.play()
                setmute()
                getfilename(medialist(itmindex))
                If playistopmost Then
                    Me.TopMost = True
                End If
            Else
                If itmindex = UBound(medialist) Then
                    newitm = 0
                Else
                    newitm = itmindex
                End If
                Deleteitemfromarray(itmindex)
                currentsong(, newitm)
            End If

        Catch ex As Exception

        End Try
    End Sub

    Private Sub nextsong1()
        Try
            Dim olditm As Integer = itmindex
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
            setcurrentrow(itmindex, olditm)
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
            Dim olditm As Integer = itmindex
            itmindex = UBound(medialist)
            setcurrentrow(itmindex, olditm)
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

    Public Sub setmute(Optional mflag As Integer = 0)
        If 静音MToolStripMenuItem.Checked Then
            AxWindowsMediaPlayer1.settings.mute = True
            If mflag = 1 Then
                Timer5.Enabled = False
                Timer4.Enabled = False
                lbl_1.Text = "媒体静音"
                Timer4.Enabled = True
            End If
        Else
            AxWindowsMediaPlayer1.settings.mute = False
            If mflag = 1 Then
                Timer5.Enabled = False
                Timer4.Enabled = False
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
                停止循环ToolStripMenuItem.Checked = False
                isautoloop = True
                looptype = 0
            Case 1
                单曲循环ToolStripMenuItem.Checked = False
                有序循环ToolStripMenuItem1.Checked = True
                无序训话ToolStripMenuItem.Checked = False
                停止循环ToolStripMenuItem.Checked = False
                isautoloop = True
                looptype = 1
            Case 2
                单曲循环ToolStripMenuItem.Checked = False
                有序循环ToolStripMenuItem1.Checked = False
                无序训话ToolStripMenuItem.Checked = True
                停止循环ToolStripMenuItem.Checked = False
                isautoloop = True
                looptype = 2
            Case 3
                单曲循环ToolStripMenuItem.Checked = False
                有序循环ToolStripMenuItem1.Checked = False
                无序训话ToolStripMenuItem.Checked = False
                停止循环ToolStripMenuItem.Checked = True
                isautoloop = False
                looptype = 3
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
        Try
            If form2show Then
                'setform2position()
            End If
        Catch ex As Exception

        End Try
    End Sub

    Private Sub Timer2_Tick(sender As Object, e As EventArgs) Handles Timer2.Tick

        Try
            If AxWindowsMediaPlayer1.playState = WMPLib.WMPPlayState.wmppsPlaying Or WMPLib.WMPPlayState.wmppsPaused Then
                If AxWindowsMediaPlayer1.Ctlcontrols.currentPositionString = "" Then
                    If (AxWindowsMediaPlayer1.currentMedia.durationString.ToString.Length) >= 5 Then
                        If is_show_lrc Then
                            Me.Text = Current_play_name & " | 00:00 | " & AxWindowsMediaPlayer1.currentMedia.durationString '& "|" & AxWindowsMediaPlayer1.playState
                        Else
                            Me.Text = "00:00 | " & AxWindowsMediaPlayer1.currentMedia.durationString '& "|" & AxWindowsMediaPlayer1.playState
                        End If

                        Label1.Text = Me.Text
                        'lbl_1.Text = "00:00 | " & AxWindowsMediaPlayer1.currentMedia.durationString
                    Else
                        If is_show_lrc Then
                            Me.Text = Current_play_name & " | 00:00:00 | " & AxWindowsMediaPlayer1.currentMedia.durationString '& "|" & AxWindowsMediaPlayer1.playState
                        Else
                            Me.Text = "00:00:00 | " & AxWindowsMediaPlayer1.currentMedia.durationString '& "|" & AxWindowsMediaPlayer1.playState
                        End If
                        Label1.Text = Me.Text
                        'lbl_1.Text = "00:00:00 | " & AxWindowsMediaPlayer1.currentMedia.durationString
                    End If
                Else
                    If is_show_lrc Then
                        Me.Text = Current_play_name & " | " & AxWindowsMediaPlayer1.Ctlcontrols.currentPositionString & " | " & AxWindowsMediaPlayer1.currentMedia.durationString '& "|" & AxWindowsMediaPlayer1.playState
                    Else
                        Me.Text = AxWindowsMediaPlayer1.Ctlcontrols.currentPositionString & " | " & AxWindowsMediaPlayer1.currentMedia.durationString '& "|" & AxWindowsMediaPlayer1.playState
                    End If
                    Label1.Text = Me.Text
                    'lbl_1.Text = AxWindowsMediaPlayer1.Ctlcontrols.currentPositionString & " | " & AxWindowsMediaPlayer1.currentMedia.durationString
                End If
                If AxWindowsMediaPlayer1.playState = 1 Then
                    Me.Text = "Player" '& "|" & isautoloop  '& "|" & AxWindowsMediaPlayer1.playState '"00:00 | 00:00"
                End If
                Label1.Text = Me.Text
            Else
                Me.Text = "Player" '& "|" & isautoloop '& "|" & AxWindowsMediaPlayer1.playState
                showplaystatestr(Label1)
            End If

        Catch ex As Exception
            Me.Text = "Player" '& "|" & AxWindowsMediaPlayer1.playState
            'Label1.Text = AxWindowsMediaPlayer1.playState
            showplaystatestr(Label1)
        End Try
        'Dim ka As New AudioMixerHelper
        'lbl_1.Text = ka.GetVolume()
        'lbl_1.Text = medialist.LongLength
    End Sub

    Public Sub showplaystatestr(ByVal lbl As Label)
        Select Case AxWindowsMediaPlayer1.playState
            Case 0
                lbl.Text = "未定义状态"
            Case 1
                lbl.Text = "停止"
            Case 2
                lbl.Text = "暂停"
            Case 3
                lbl.Text = "播放"
            Case 4
                lbl.Text = "ScanForward"
            Case 5
                lbl.Text = "ScanReverse"
            Case 6
                lbl.Text = "Buffering"
            Case 7
                lbl.Text = "Waiting"
            Case 8
                lbl.Text = "MediaEnded"
            Case 9
                lbl.Text = "Transitioning"
            Case 10
                lbl.Text = "Ready"
            Case 11
                lbl.Text = "Reconnecting"
            Case 12
                lbl.Text = "Last"
            Case Else
                lbl.Text = "未知"
        End Select
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
            If 通透模式ToolStripMenuItem.Checked Then
                Me.TransparencyKey = Nothing
            End If
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
            If 通透模式ToolStripMenuItem.Checked Then
                Me.TransparencyKey = Color.Black
            End If
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
        If 显示播放进度控制面板PToolStripMenuItem.Checked Then
            If 显示状态栏ToolStripMenuItem.Checked Then
                Panel3.Height = Panel2.Height + Panel1.Height
            Else
                Panel3.Height = Panel1.Height
            End If
        Else
            If 显示状态栏ToolStripMenuItem.Checked Then
                Panel3.Height = Panel2.Height
            Else
                Panel3.Height = 0
            End If
        End If
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
                If AxWindowsMediaPlayer1.playState = 1 Then
                    lbl_1.Text = "" ' "00:00 | 00:00"
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
        'Me.Panel5.Visible = 窗体无框模式ToolStripMenuItem.Checked
        setform2position()
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
        Timer4.Enabled = False
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
        Timer4.Enabled = False
        lbl_1.Text = "媒体音量 " & media_c_volumn & " "
        Timer4.Enabled = True
    End Sub

    Private Sub 最大ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 最大ToolStripMenuItem.Click
        media_c_volumn = 100
        AxWindowsMediaPlayer1.settings.volume = media_c_volumn
        Timer5.Enabled = False
        Timer4.Enabled = False
        lbl_1.Text = "媒体音量 " & media_c_volumn & " "
        Timer4.Enabled = True
    End Sub

    Private Sub 居中ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 居中ToolStripMenuItem.Click
        media_c_volumn = 50
        AxWindowsMediaPlayer1.settings.volume = media_c_volumn
        Timer5.Enabled = False
        Timer4.Enabled = False
        lbl_1.Text = "媒体音量 " & media_c_volumn & " "
        Timer4.Enabled = True
    End Sub

    Private Sub 最小ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 最小ToolStripMenuItem.Click
        media_c_volumn = 0
        AxWindowsMediaPlayer1.settings.volume = media_c_volumn
        Timer5.Enabled = False
        Timer4.Enabled = False
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

    Private Sub 显示播放列表ToolStripMenuItem1_Click(sender As Object, e As EventArgs) Handles 显示播放列表ToolStripMenuItem1.Click
        显示播放列表ToolStripMenuItem1.Checked = Not 显示播放列表ToolStripMenuItem1.Checked
        'If 显示播放列表ToolStripMenuItem1.Checked Then
        'Dim fm1 As New Form2
        'If form2show Then
        'Form2.Dispose()
        'Form2.Close()
        'Else
        showsonglist()
        'End If

    End Sub

    Private Sub showsonglist()
        If form2show = False Then
            Form2.Hide()
            Form2.FormBorderStyle = FormBorderStyle.None
            Form2.StartPosition = FormStartPosition.Manual
            If 窗体无框模式ToolStripMenuItem.Checked Then
                Form2.Top = Me.Top + Me.Height
                Form2.Left = Me.Left
                Form2.Width = Me.Width
            Else
                Form2.Top = Me.Top + Me.Height - 7
                Form2.Left = Me.Left + 7
                Form2.Width = Me.Width - 14
            End If
            Form2.Height = 150
            setform2position()

            Form2.Show(Me)
            form2show = True
            Me.Focus()
            显示播放列表ToolStripMenuItem1.Checked = True
        Else
            form2show = False
            Form2.Dispose()
            Form2.Close()
            显示播放列表ToolStripMenuItem1.Checked = False
        End If
    End Sub

    Private Sub setform2position()
        If form2show Then
            If 窗体无框模式ToolStripMenuItem.Checked Then
                Form2.Top = Me.Top + Me.Height
                Form2.Left = Me.Left
                Form2.Width = Me.Width
                Form2.Height = 150
            Else
                Form2.Top = Me.Top + Me.Height - 7
                Form2.Left = Me.Left + 7
                Form2.Width = Me.Width - 14
                Form2.Height = 150
            End If

        End If
    End Sub

    Public Sub setcurrentrow(ByVal citm As Integer, ByVal olditm As Integer)
        If form2show Then
            Form2.DataGridView1.CurrentCell = Form2.DataGridView1.Rows(citm).Cells(0)
            Form2.DataGridView1.Rows(citm).Cells(2).Value = "Playing"
            Form2.DataGridView1.Rows(olditm).Cells(2).Value = ""

            Form2.DataGridView1.Rows(citm).Cells(0).Style.ForeColor = Color.FromArgb(12, 33, 60)
            Form2.DataGridView1.Rows(citm).Cells(1).Style.ForeColor = Color.FromArgb(12, 33, 60)
            Form2.DataGridView1.Rows(citm).Cells(2).Style.ForeColor = Color.FromArgb(12, 33, 60)
            Form2.DataGridView1.Rows(citm).Cells(0).Style.BackColor = Color.Orange
            Form2.DataGridView1.Rows(citm).Cells(1).Style.BackColor = Color.Orange
            Form2.DataGridView1.Rows(citm).Cells(2).Style.BackColor = Color.Orange

            Form2.DataGridView1.Rows(olditm).Cells(0).Style.ForeColor = Color.White
            Form2.DataGridView1.Rows(olditm).Cells(1).Style.ForeColor = Color.White
            Form2.DataGridView1.Rows(olditm).Cells(2).Style.ForeColor = Color.White
            Form2.DataGridView1.Rows(olditm).Cells(0).Style.BackColor = Color.FromArgb(12, 33, 60)
            Form2.DataGridView1.Rows(olditm).Cells(1).Style.BackColor = Color.FromArgb(12, 33, 60)
            Form2.DataGridView1.Rows(olditm).Cells(2).Style.BackColor = Color.FromArgb(12, 33, 60)
        End If

    End Sub

    Private Sub Panel2_MouseDown(sender As Object, e As MouseEventArgs) Handles Panel2.MouseDown
        Select Case e.Button
            Case Windows.Forms.MouseButtons.Right
                'Me.ContextMenuStrip1.Show(e.X + Me.Left + 9, e.Y + Me.Top + 32)
                Me.ContextMenuStrip1.Show(MousePosition.X, MousePosition.Y)
        End Select
    End Sub

    Private Sub Form1_LocationChanged(sender As Object, e As EventArgs) Handles Me.LocationChanged
        If form2show Then
            setform2position()
        End If
    End Sub

    Private Sub Panel4_Paint(sender As Object, e As PaintEventArgs) Handles Panel4.Paint

    End Sub

    Private Sub Form1_Resize(sender As Object, e As EventArgs) Handles Me.Resize
        On Error Resume Next
        setform2position()
    End Sub

    Private Sub Label1_Click(sender As Object, e As EventArgs) Handles Label1.Click

    End Sub

    Private Sub Panel4_MouseDown(sender As Object, e As MouseEventArgs) Handles Panel4.MouseDown
        On Error Resume Next
        Me.ContextMenuStrip1.Show(MousePosition.X, MousePosition.Y)
    End Sub

    Private Sub Form1_FormClosed(sender As Object, e As FormClosedEventArgs) Handles Me.FormClosed
        On Error Resume Next
        saveplaylisttofile()
    End Sub

    Private Sub 停止循环ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 停止循环ToolStripMenuItem.Click
        setloop(3)
    End Sub

    Private Sub 通透模式ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 通透模式ToolStripMenuItem.Click
        通透模式ToolStripMenuItem.Checked = Not 通透模式ToolStripMenuItem.Checked
        If 通透模式ToolStripMenuItem.Checked = True Then
            Me.TransparencyKey = Color.Black
        Else
            Me.TransparencyKey = Nothing
        End If
    End Sub

    Private Sub 显示状态栏ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 显示状态栏ToolStripMenuItem.Click
        showorhidestatusbar()
    End Sub

    Sub showorhidestatusbar()
        显示状态栏ToolStripMenuItem.Checked = Not 显示状态栏ToolStripMenuItem.Checked
        Panel2.Visible = 显示状态栏ToolStripMenuItem.Checked
        If 显示状态栏ToolStripMenuItem.Checked Then
            If 显示播放进度控制面板PToolStripMenuItem.Checked Then
                Panel3.Height = Panel2.Height + Panel1.Height
            Else
                Panel3.Height = Panel1.Height
            End If
        Else
            If 显示播放进度控制面板PToolStripMenuItem.Checked Then
                Panel3.Height = Panel1.Height
            Else
                Panel3.Height = 0
            End If
        End If
    End Sub

    Private Sub Form1_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        On Error Resume Next
        saveplaylisttofile()
    End Sub



    Private Sub Show_lrc()
        Dim m_str = Split(medialist(itmindex), ".")
        Dim l_str As String = m_str(UBound(m_str) - 1) & ".lrc"
        Dim line As String = ""
        If System.IO.File.Exists(l_str) Then
            'TextBox1.Text = ""
            'TextBox2.Text = ""
            Dim sr As StreamReader = New StreamReader(l_str, System.Text.Encoding.UTF8)
            Dim rgx As Regex = New Regex("^\[[0-9]")
            Dim rgx1 As Regex = New Regex("^\[[a-zA-Z]")
            Dim kl
            Dim lrc_str As String = ""
            Dim line_count As Integer = -1
            Dim tmp_1 As String = ""
            Dim tmp
            offset_value = 0
            Do While sr.Peek() > 0
                line = sr.ReadLine
                If Trim(line & "") <> "" Then
                    '歌曲信息部分，包括offset
                    If rgx1.Match(line).Success Then
                        'pianyi
                        kl = Split(line, "]")
                        tmp_1 = Replace(kl(0), "[", "")
                        tmp = tmp_1.Split(":")
                        Select Case LCase(tmp(0) & "")
                            Case "offset"
                                offset_value = tmp(1) / 1000
                        End Select
                    End If
                    '歌词部分
                    If rgx.Match(line).Success Then
                        kl = Split(line, "]")
                        line_count = line_count + 1
                        For i As Integer = 0 To UBound(kl) - 1
                            If lrc_str = "" Then
                                lrc_str = lrc_str & Replace(kl(i), "[", "") & "[xhb]" & kl(UBound(kl))
                            Else
                                lrc_str = lrc_str & vbCrLf & Replace(kl(i), "[", "") & "[xhb]" & kl(UBound(kl))
                            End If
                        Next
                    End If
                End If
            Loop
            sr.Close()
            sr = Nothing
            'ReDim lrc_array(line_count, 1)
            Dim pl = Split(lrc_str, vbCrLf)
            Dim op

            ReDim lrc_array(UBound(pl), 1)
            For j As Integer = 0 To UBound(pl)
                op = Split(pl(j), "[xhb]")
                lrc_array(j, 0) = op(0)
                lrc_array(j, 1) = op(1)
            Next
            If line_count > 0 Then
                is_show_lrc = True
                Timer6.Enabled = True
            End If
            'MsgBox(line_count & "/" & UBound(pl) & "/" & lrc_array.Length)
        Else
            Timer6.Enabled = False
            is_show_lrc = False
        End If
    End Sub

    Private Sub Timer6_Tick(sender As Object, e As EventArgs) Handles Timer6.Tick
        If is_show_lrc Then
            If Me.AxWindowsMediaPlayer1.playState = WMPLib.WMPPlayState.wmppsPlaying Then
                start_show_lrc(lrc_array, Math.Round(AxWindowsMediaPlayer1.Ctlcontrols.currentPosition, 2))
            End If
        End If
    End Sub

    Private Sub start_show_lrc(ByVal lrc_ary As Array, ByVal cnt As Integer)
        Dim kl
        Dim mm
        Dim c_t_in As Integer = 0
        Dim a_info As String = ""
        For i As Integer = 0 To (lrc_ary.Length / 2 - 1)
            kl = Split(lrc_ary(i, 0), ".")
            mm = Split(kl(0), ":")
            Select Case UBound(mm)
                Case 1
                    c_t_in = Int(mm(0)) * 60 + Int(mm(1)) + (kl(UBound(kl)) / 100)
                Case 2
                    c_t_in = Int(mm(0)) * 3600 + Int(mm(1)) * 60 + Int(mm(2)) + (kl(UBound(kl)) / 100)
            End Select
            c_t_in = c_t_in + offset_value
            If (c_t_in) = (cnt) Then
                a_info = lrc_ary(i, 1)
                'Exit For
            End If
        Next
        If a_info <> "" Then
            lbl.Text = a_info
        End If
    End Sub
End Class
