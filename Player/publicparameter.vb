Module publicparameter
    Public medialist() As String
    Public itmindex As Integer = -1
    Public looptype As Integer = 1
    Public playistopmost As Boolean = False
    Public form2show As Boolean = False
    Public move_item_index As Integer = -1
    Public playlist As String = Application.StartupPath & "\Playlist.ply"

    Public key_str As String = "Q3Es5Z02"  'DES加解密密钥 key "Q3Es5Z02"
    Public iv_str As String = "pUy8G6M2"   'DES加解密初始化向量 IV "pUy8G6M2"


    '密码解密函数 使用DES对称解密
    Public Function DecryptDes(ByVal SourceStr As String, ByVal myKey As String, ByVal myIV As String) As String    '使用标准DES对称解密
        Dim des As New System.Security.Cryptography.DESCryptoServiceProvider 'DES算法
        'Dim des As New System.Security.Cryptography.TripleDESCryptoServiceProvider 'TripleDES算法
        des.Key = System.Text.Encoding.UTF8.GetBytes(myKey) 'myKey DES用8个字符，TripleDES要24个字符
        des.IV = System.Text.Encoding.UTF8.GetBytes(myIV) 'myIV DES用8个字符，TripleDES要8个字符
        Dim buffer As Byte() = Convert.FromBase64String(SourceStr)
        Dim ms As New System.IO.MemoryStream(buffer)
        Dim cs As New System.Security.Cryptography.CryptoStream(ms, des.CreateDecryptor(), System.Security.Cryptography.CryptoStreamMode.Read)
        Dim sr As New System.IO.StreamReader(cs)
        DecryptDes = sr.ReadToEnd()
    End Function

    '密码加密函数 使用DES对称加密
    Public Function EncryptDes(ByVal SourceStr As String, ByVal myKey As String, ByVal myIV As String) As String '使用的DES对称加密
        Dim des As New System.Security.Cryptography.DESCryptoServiceProvider 'DES算法
        'Dim des As New System.Security.Cryptography.TripleDESCryptoServiceProvider 'TripleDES算法
        Dim inputByteArray As Byte()
        inputByteArray = System.Text.Encoding.Default.GetBytes(SourceStr)
        des.Key = System.Text.Encoding.UTF8.GetBytes(myKey) 'myKey DES用8个字符，TripleDES要24个字符
        des.IV = System.Text.Encoding.UTF8.GetBytes(myIV) 'myIV DES用8个字符，TripleDES要8个字符
        Dim ms As New System.IO.MemoryStream
        Dim cs As New System.Security.Cryptography.CryptoStream(ms, des.CreateEncryptor(), System.Security.Cryptography.CryptoStreamMode.Write)
        Dim sw As New System.IO.StreamWriter(cs)
        sw.Write(SourceStr)
        sw.Flush()
        cs.FlushFinalBlock()
        ms.Flush()
        EncryptDes = Convert.ToBase64String(ms.GetBuffer(), 0, ms.Length)

    End Function

    Public Sub Deleteitemfromarray(ByVal delitemindexs As Integer)
        Dim n_list As New ListBox
        n_list.Items.Clear()
        move_item_index = -1
        For i As Integer = 0 To UBound(medialist)
            If i = delitemindexs Then
            Else
                n_list.Items.Add(medialist(i))
            End If
        Next
        ReDim medialist(n_list.Items.Count - 1)
        For j As Integer = 0 To n_list.Items.Count - 1
            medialist(j) = n_list.Items(j)
        Next
        If delitemindexs = itmindex Then
            Form1.stopmedia()

            If n_list.Items.Count = 0 Then
                itmindex = -1
                Form1.Text = ""
                Form1.lbl.Text = "就绪..."
                Form1.lbl_1.Text = ""
            Else
                If n_list.Items.Count = 1 Then
                    itmindex = 0
                Else
                    If itmindex + 1 <= n_list.Items.Count Then
                        itmindex = itmindex
                    Else
                        itmindex = 0
                    End If
                End If
                Form1.AxWindowsMediaPlayer1.URL = medialist(itmindex)
                Form1.AxWindowsMediaPlayer1.Ctlcontrols.play()
                Form1.getfilename(medialist(itmindex))
            End If
        ElseIf itmindex > delitemindexs Then
            itmindex = itmindex - 1
        End If

        saveplaylisttofile()
        If form2show Then
            Form2.getplaylist()
        End If
    End Sub

    Public Sub saveplaylisttofile()

        Dim FS As New System.IO.FileStream(playlist, IO.FileMode.Create)
        Dim Bw As New System.IO.BinaryWriter(FS, System.Text.Encoding.Unicode)
        Dim str As String = ""
        Dim cr_postion As Integer = 0
        Try
            For i As Integer = 0 To UBound(medialist)
                If str = "" Then
                    str = EncryptDes(medialist(i).ToString.Trim, key_str, iv_str)
                Else
                    str = str & vbCrLf & EncryptDes(medialist(i).ToString.Trim, key_str, iv_str)
                End If
            Next
            If Form1.AxWindowsMediaPlayer1.playState = WMPLib.WMPPlayState.wmppsPlaying Or WMPLib.WMPPlayState.wmppsPaused Then
                cr_postion = Form1.AxWindowsMediaPlayer1.Ctlcontrols.currentPosition
            Else
                cr_postion = 0
            End If
            str = str & vbCrLf & EncryptDes(itmindex & ":" & cr_postion, key_str, iv_str)
        Catch ex As Exception
            str = ""
        End Try
        'EncryptDes(str, key_str, iv_str))
        Bw.Write(str)
        Bw.Close() '记住，操作完成后及时关闭Bw，否则可能破坏文件
        FS.Close()
    End Sub
End Module
