Module publicparameter
    Public medialist() As String
    Public itmindex As Integer = -1
    Public looptype As Integer = 1
    Public playistopmost As Boolean = False
    Public form2show As Boolean = False

    Public playlist As String = Application.StartupPath & "\Playlist.ply"


    Public Sub Deleteitemfromarray(ByVal itemindex As Integer)
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
        If form2show Then
            Form2.getplaylist()
        End If
    End Sub

    Public Sub saveplaylisttofile()
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
End Module
