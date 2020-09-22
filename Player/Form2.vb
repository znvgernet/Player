Imports System.Threading
Imports System.Threading.Thread

Public Class Form2
    Public X, Y As Integer
    Public mouse_click_cell_row_index As Integer = -1

    Private Sub Form2_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Me.BackColor = Color.FromArgb(12, 33, 60)
        Panel1.BackColor = Color.FromArgb(12, 33, 60)
        Panel2.BackColor = Color.FromArgb(12, 33, 60)
        Panel3.BackColor = Color.FromArgb(12, 33, 60)
        Me.DataGridView1.BackgroundColor = Color.FromArgb(12, 33, 60)
        Me.DataGridView1.RowsDefaultCellStyle.BackColor = Color.FromArgb(12, 33, 60)
        Me.DataGridView1.AlternatingRowsDefaultCellStyle.BackColor = Color.FromArgb(12, 33, 60)
        form2show = True
        Label1.Dock = DockStyle.Fill
        Label3.Dock = DockStyle.Fill
        Panel3.Dock = DockStyle.Fill
        DataGridView1.Dock = DockStyle.Fill


        AddHandler Label1.MouseDown, AddressOf lbl_MouseDown '委托拖放数据事件
        AddHandler Label1.MouseMove, AddressOf lbl_MouseMove '委托数据处理事件

        AddHandler Label3.MouseDown, AddressOf lbl_MouseDown '委托拖放数据事件
        AddHandler Label3.MouseMove, AddressOf lbl_MouseMove '委托数据处理事件
        AddHandler Label4.MouseDown, AddressOf lbl_MouseDown '委托拖放数据事件
        AddHandler Label4.MouseMove, AddressOf lbl_MouseMove '委托数据处理事件

        getplaylist()
        changedataviewsize()
    End Sub

    Public Sub getplaylist()
        Try
            DataGridView1.Rows.Clear()
            If UBound(medialist) >= 0 Then
                For i As Integer = 0 To UBound(medialist)
                    i = DataGridView1.Rows.Add()
                    DataGridView1.Rows(i).Cells(0).Value = i + 1
                    DataGridView1.Rows(i).Cells(1).Value = getfilename(medialist(i))
                    DataGridView1.Rows(i).Cells(1).ToolTipText = medialist(i)
                    DataGridView1.Rows(i).Cells(1).Style.Alignment = DataGridViewContentAlignment.MiddleLeft
                    If i = itmindex Then
                        DataGridView1.Rows(i).Cells(2).Value = "Playing"
                    Else
                        DataGridView1.Rows(i).Cells(2).Value = ""
                    End If
                Next
                DataGridView1.CurrentCell = DataGridView1.Rows(itmindex).Cells(0)
                DataGridView1.Rows(itmindex).Cells(0).Style.ForeColor = Color.FromArgb(12, 33, 60)
                DataGridView1.Rows(itmindex).Cells(1).Style.ForeColor = Color.FromArgb(12, 33, 60)
                DataGridView1.Rows(itmindex).Cells(2).Style.ForeColor = Color.FromArgb(12, 33, 60)
                DataGridView1.Rows(itmindex).Cells(0).Style.BackColor = Color.Orange
                DataGridView1.Rows(itmindex).Cells(1).Style.BackColor = Color.Orange
                DataGridView1.Rows(itmindex).Cells(2).Style.BackColor = Color.Orange
                move_item_index = itmindex
            End If
        Catch ex As Exception
            'MsgBox(ex.Message)
        End Try

    End Sub

    Private Sub playcurrrowsong(ByVal citm As Integer)
        Try
            Dim olditm As Integer = itmindex
            itmindex = citm
            If itmindex > UBound(medialist) Then
                itmindex = 0
            End If
            Form1.setcurrentrow(itmindex, olditm)
            If System.IO.File.Exists(medialist(itmindex)) Then
                Form1.AxWindowsMediaPlayer1.Ctlcontrols.stop()
                Form1.AxWindowsMediaPlayer1.URL = medialist(itmindex)
                Form1.AxWindowsMediaPlayer1.Ctlcontrols.play()
                If Form1.停止循环ToolStripMenuItem.Checked = True Then
                    Form1.isautoloop = False
                Else
                    Form1.isautoloop = True
                End If
                Form1.setmute()
                Form1.getfilename(medialist(itmindex))
                If playistopmost Then
                    Me.TopMost = True
                End If
            Else
                Deleteitemfromarray(itmindex)

                Form1.nextsong()
            End If
        Catch ex As Exception

        End Try
    End Sub

    Private Function getfilename(ByVal files As String) As String
        Dim sp = Split(files, "\")
        Dim str As String = sp(UBound(sp)).ToString.Trim
        Return str
        'Dim ps = Split(str, ".")
        'Dim rts As String = ""
        'For i As Integer = 0 To UBound(ps) - 1
        '    If rts = "" Then
        '        rts = ps(i)
        '    Else
        '        rts = rts & "." & ps(i)
        '    End If
        'Next


    End Function

    Private Sub Label2_Click(sender As Object, e As EventArgs) Handles Label2.Click
        Me.Dispose()
        Me.Close()
        form2show = False
        Form1.显示播放列表ToolStripMenuItem1.Checked = False
    End Sub

    Private Sub lbl_MouseDown(sender As Object, e As MouseEventArgs)
        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                X = e.X : Y = e.Y
            Case Windows.Forms.MouseButtons.Right
                'Me.ContextMenuStrip1.Show(e.X + Me.Left + 9, e.Y + Me.Top + 32)
        End Select
    End Sub

    Private Sub lbl_MouseMove(sender As Object, e As MouseEventArgs)
        On Error Resume Next
        If X = e.X And Y = e.Y Then Exit Sub
        If e.Button = Windows.Forms.MouseButtons.Left Then
            Me.Left = Me.Left + e.X - X
            Me.Top = Me.Top + e.Y - Y
            setform1postion()
            Form1.Focus()
        End If
    End Sub

    Private Sub setform1postion()
        If Form1.窗体无框模式ToolStripMenuItem.Checked Then
            Form1.Top = Me.Top - Form1.Height
            Form1.Left = Me.Left
        Else
            Form1.Top = Me.Top - Form1.Height + 7
            Form1.Left = Me.Left - 7
        End If

    End Sub

    Private Sub Label2_MouseHover(sender As Object, e As EventArgs) Handles Label2.MouseHover
        Label2.BackColor = Color.Red
        Label2.ForeColor = Color.Yellow
    End Sub

    Private Sub Label2_MouseLeave(sender As Object, e As EventArgs) Handles Label2.MouseLeave
        Label2.BackColor = Me.BackColor
        Label2.ForeColor = Color.White
    End Sub

    Private Sub changedataviewsize()
        On Error Resume Next
        If DataGridView1.ColumnCount > 1 Then
            DataGridView1.Columns(0).Width = 30
            DataGridView1.Columns(2).Width = 60
            DataGridView1.Columns(1).Width = DataGridView1.Width - DataGridView1.Columns(0).Width - DataGridView1.Columns(2).Width - 20
        End If
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        'Label1.Text = Me.Top & "|" & Form1.Top & "/" & Me.Left & "|" & Form1.Left & "/" & Me.Width & "|" & Form1.Width
    End Sub

    Private Sub Form2_Resize(sender As Object, e As EventArgs) Handles Me.Resize
        changedataviewsize()
    End Sub

    Private Sub 从播放列表中删除ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 从播放列表中删除ToolStripMenuItem.Click
        If MsgBox("是否确定要将当前选择曲目 """ & DataGridView1.Rows(mouse_click_cell_row_index).Cells(1).Value & """ 从播放列表中删除", vbYesNo + vbQuestion, "确认") = vbYes Then
            Deleteitemfromarray(mouse_click_cell_row_index)
        End If
    End Sub

    Private Sub DataGridView1_CellDoubleClick(sender As Object, e As DataGridViewCellEventArgs) Handles DataGridView1.CellDoubleClick
        Sleep(200)
        playcurrrowsong(e.RowIndex)
    End Sub

    Private Sub 清空播放列表ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 清空播放列表ToolStripMenuItem.Click
        If MsgBox("是否确定要清空播放列表", vbYesNo + vbQuestion, "确认") = vbYes Then
            Form1.stopmedia()
            itmindex = -1
            Form1.lbl.Text = "就绪..."
            ReDim medialist(-1)
            DataGridView1.Rows.Clear()
            saveplaylisttofile()
            move_item_index = -1
        End If
    End Sub

    Private Sub DataGridView1_CellMouseDown(sender As Object, e As DataGridViewCellMouseEventArgs) Handles DataGridView1.CellMouseDown
        On Error Resume Next
        If e.Button = Windows.Forms.MouseButtons.Right Then
            DataGridView1.CurrentCell = DataGridView1.Rows(e.RowIndex).Cells(0)
            mouse_click_cell_row_index = e.RowIndex
            'Label1.Text = mouse_click_cell_row_index
            If e.RowIndex = 0 Then
                向上移动ToolStripMenuItem.Enabled = False
                向下移动ToolStripMenuItem.Enabled = True
            ElseIf e.RowIndex = DataGridView1.RowCount - 1 Then
                向上移动ToolStripMenuItem.Enabled = True
                向下移动ToolStripMenuItem.Enabled = False
            Else
                向上移动ToolStripMenuItem.Enabled = True
                向下移动ToolStripMenuItem.Enabled = True
            End If
            move_item_index = e.RowIndex
            Me.ContextMenuStrip1.Show(MousePosition.X, MousePosition.Y)
        End If
    End Sub

    Private Sub Form2_GotFocus(sender As Object, e As EventArgs) Handles Me.GotFocus
        On Error Resume Next
        Form1.Focus()
    End Sub

    Private Sub DataGridView1_Click(sender As Object, e As EventArgs) Handles DataGridView1.Click
        On Error Resume Next
        Form1.Focus()
    End Sub

    Private Sub 向上移动ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 向上移动ToolStripMenuItem.Click
        On Error Resume Next
        moveitemposition(mouse_click_cell_row_index, 1)
    End Sub

    Private Sub 向下移动ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles 向下移动ToolStripMenuItem.Click
        On Error Resume Next
        moveitemposition(mouse_click_cell_row_index, 2)
    End Sub


    Public Sub moveitemposition(ByVal itmx As Integer, ByVal directflg As Integer)
        Dim move_song_base As String
        Dim move_song_noth As String
        Dim move_txt_1 As String
        Dim move_txt_2 As String
        Dim move_txt_3 As String
        Dim move_txt_4 As String
        Dim new_itx As Integer = -1
        move_song_base = medialist(itmx)
        'itmindex
        Select Case directflg
            Case 1  '向上移动
                If itmx = 0 Then
                    '播放列表顶部了不能再向上移动
                    Exit Sub
                End If
                new_itx = itmx - 1
            Case 2  '向下移动
                If itmx = DataGridView1.RowCount - 1 Then
                    '播放列表底部了不能再向下移动
                    Exit Sub
                End If
                new_itx = itmx + 1
        End Select
        move_song_noth = medialist(new_itx)
        medialist(new_itx) = move_song_base
        medialist(itmx) = move_song_noth

        move_txt_1 = DataGridView1.Rows(itmx).Cells(1).Value
        move_txt_2 = DataGridView1.Rows(itmx).Cells(1).ToolTipText
        move_txt_3 = DataGridView1.Rows(new_itx).Cells(1).Value
        move_txt_4 = DataGridView1.Rows(new_itx).Cells(1).ToolTipText

        DataGridView1.Rows(itmx).Cells(1).Value = move_txt_3
        DataGridView1.Rows(itmx).Cells(1).ToolTipText = move_txt_4
        DataGridView1.Rows(new_itx).Cells(1).Value = move_txt_1
        DataGridView1.Rows(new_itx).Cells(1).ToolTipText = move_txt_2


        If itmx = itmindex Or new_itx = itmindex Then
            DataGridView1.Rows(itmindex).Cells(0).Style.ForeColor = Color.White
            DataGridView1.Rows(itmindex).Cells(1).Style.ForeColor = Color.White
            DataGridView1.Rows(itmindex).Cells(2).Style.ForeColor = Color.White
            DataGridView1.Rows(itmindex).Cells(0).Style.BackColor = Color.FromArgb(12, 33, 60)
            DataGridView1.Rows(itmindex).Cells(1).Style.BackColor = Color.FromArgb(12, 33, 60)
            DataGridView1.Rows(itmindex).Cells(2).Style.BackColor = Color.FromArgb(12, 33, 60)
            DataGridView1.Rows(itmindex).Cells(2).Value = ""
            If itmx = itmindex Then
                DataGridView1.CurrentCell = DataGridView1.Rows(new_itx).Cells(0)
                itmindex = new_itx
            Else
                DataGridView1.CurrentCell = DataGridView1.Rows(new_itx).Cells(0)
                If directflg = 1 Then
                    itmindex = itmindex + 1
                ElseIf directflg = 2 Then
                    itmindex = itmindex - 1
                End If
            End If
            DataGridView1.Rows(itmindex).Cells(0).Style.ForeColor = Color.FromArgb(12, 33, 60)
            DataGridView1.Rows(itmindex).Cells(1).Style.ForeColor = Color.FromArgb(12, 33, 60)
            DataGridView1.Rows(itmindex).Cells(2).Style.ForeColor = Color.FromArgb(12, 33, 60)
            DataGridView1.Rows(itmindex).Cells(0).Style.BackColor = Color.Orange
            DataGridView1.Rows(itmindex).Cells(1).Style.BackColor = Color.Orange
            DataGridView1.Rows(itmindex).Cells(2).Style.BackColor = Color.Orange
            DataGridView1.Rows(itmindex).Cells(2).Value = "Playing"
        End If
        DataGridView1.CurrentCell = DataGridView1.Rows(new_itx).Cells(0)
        DataGridView1.Refresh()
        saveplaylisttofile()
    End Sub


    Private Sub Form2_FormClosed(sender As Object, e As FormClosedEventArgs) Handles Me.FormClosed
        move_item_index = -1
    End Sub
End Class