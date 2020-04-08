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
        End If
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
                Form1.AxWindowsMediaPlayer1.URL = medialist(itmindex)
                Form1.AxWindowsMediaPlayer1.Ctlcontrols.play()
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
            Form1.Left = Me.Left - 8
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
        End If
    End Sub

    Private Sub DataGridView1_CellMouseDown(sender As Object, e As DataGridViewCellMouseEventArgs) Handles DataGridView1.CellMouseDown
        On Error Resume Next
        If e.Button = Windows.Forms.MouseButtons.Right Then
            DataGridView1.CurrentCell = DataGridView1.Rows(e.RowIndex).Cells(0)
            mouse_click_cell_row_index = e.RowIndex
            'Label1.Text = mouse_click_cell_row_index
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

    Private Sub Form2_LocationChanged(sender As Object, e As EventArgs) Handles Me.LocationChanged
        On Error Resume Next
        'setform1postion()
    End Sub
End Class