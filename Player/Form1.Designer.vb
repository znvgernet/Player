﻿<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class Form1
    Inherits System.Windows.Forms.Form

    'Form 重写 Dispose，以清理组件列表。
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Windows 窗体设计器所必需的
    Private components As System.ComponentModel.IContainer

    '注意:  以下过程是 Windows 窗体设计器所必需的
    '可以使用 Windows 窗体设计器修改它。  
    '不要使用代码编辑器修改它。
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Form1))
        Me.Timer1 = New System.Windows.Forms.Timer(Me.components)
        Me.ContextMenuStrip1 = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.ToolStripMenuItem2 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem3 = New System.Windows.Forms.ToolStripSeparator()
        Me.播放暂停ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.停止ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem1 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripMenuItem7 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.第一个ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.上一个节目ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.下一个节目ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.最后一个节目ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem4 = New System.Windows.Forms.ToolStripSeparator()
        Me.单个循环ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.单曲循环ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem14 = New System.Windows.Forms.ToolStripSeparator()
        Me.有序循环ToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.无序训话ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem5 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripMenuItem9 = New System.Windows.Forms.ToolStripMenuItem()
        Me.静音MToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem12 = New System.Windows.Forms.ToolStripSeparator()
        Me.增大ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.减小ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem13 = New System.Windows.Forms.ToolStripSeparator()
        Me.最大ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.居中ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.最小ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.媒体声道ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.立体声ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem6 = New System.Windows.Forms.ToolStripSeparator()
        Me.左声道ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.右声道ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem8 = New System.Windows.Forms.ToolStripSeparator()
        Me.显示播放列表ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.窗体无框模式ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem11 = New System.Windows.Forms.ToolStripSeparator()
        Me.显示播放进度控制面板PToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.显示播放列表ToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem10 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator()
        Me.窗口置顶ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.始终置顶ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.播放时置顶ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.从不置顶ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.OpenFileDialog1 = New System.Windows.Forms.OpenFileDialog()
        Me.Timer2 = New System.Windows.Forms.Timer(Me.components)
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.PictureBox3 = New System.Windows.Forms.PictureBox()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.PictureBox2 = New System.Windows.Forms.PictureBox()
        Me.PictureBox4 = New System.Windows.Forms.PictureBox()
        Me.Timer3 = New System.Windows.Forms.Timer(Me.components)
        Me.Panel2 = New System.Windows.Forms.Panel()
        Me.Timer4 = New System.Windows.Forms.Timer(Me.components)
        Me.Timer5 = New System.Windows.Forms.Timer(Me.components)
        Me.AxWindowsMediaPlayer1 = New AxWMPLib.AxWindowsMediaPlayer()
        Me.ContextMenuStrip1.SuspendLayout()
        Me.Panel1.SuspendLayout()
        CType(Me.PictureBox3, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PictureBox2, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PictureBox4, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.AxWindowsMediaPlayer1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Timer1
        '
        Me.Timer1.Enabled = True
        '
        'ContextMenuStrip1
        '
        Me.ContextMenuStrip1.ImageScalingSize = New System.Drawing.Size(20, 20)
        Me.ContextMenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripMenuItem2, Me.ToolStripMenuItem3, Me.播放暂停ToolStripMenuItem, Me.停止ToolStripMenuItem, Me.ToolStripMenuItem1, Me.ToolStripMenuItem7, Me.ToolStripSeparator1, Me.第一个ToolStripMenuItem, Me.上一个节目ToolStripMenuItem, Me.下一个节目ToolStripMenuItem, Me.最后一个节目ToolStripMenuItem, Me.ToolStripMenuItem4, Me.单个循环ToolStripMenuItem, Me.ToolStripMenuItem5, Me.ToolStripMenuItem9, Me.媒体声道ToolStripMenuItem, Me.ToolStripMenuItem8, Me.显示播放列表ToolStripMenuItem, Me.ToolStripMenuItem10, Me.ToolStripSeparator2, Me.窗口置顶ToolStripMenuItem})
        Me.ContextMenuStrip1.Name = "ContextMenuStrip1"
        Me.ContextMenuStrip1.Size = New System.Drawing.Size(202, 354)
        '
        'ToolStripMenuItem2
        '
        Me.ToolStripMenuItem2.Name = "ToolStripMenuItem2"
        Me.ToolStripMenuItem2.Size = New System.Drawing.Size(201, 22)
        Me.ToolStripMenuItem2.Text = "打开文件(&O)"
        '
        'ToolStripMenuItem3
        '
        Me.ToolStripMenuItem3.Name = "ToolStripMenuItem3"
        Me.ToolStripMenuItem3.Size = New System.Drawing.Size(198, 6)
        '
        '播放暂停ToolStripMenuItem
        '
        Me.播放暂停ToolStripMenuItem.Name = "播放暂停ToolStripMenuItem"
        Me.播放暂停ToolStripMenuItem.ShortcutKeys = CType((System.Windows.Forms.Keys.Control Or System.Windows.Forms.Keys.Space), System.Windows.Forms.Keys)
        Me.播放暂停ToolStripMenuItem.Size = New System.Drawing.Size(201, 22)
        Me.播放暂停ToolStripMenuItem.Text = "播放 | 暂停"
        '
        '停止ToolStripMenuItem
        '
        Me.停止ToolStripMenuItem.Name = "停止ToolStripMenuItem"
        Me.停止ToolStripMenuItem.Size = New System.Drawing.Size(201, 22)
        Me.停止ToolStripMenuItem.Text = "停止(&Q)"
        '
        'ToolStripMenuItem1
        '
        Me.ToolStripMenuItem1.Name = "ToolStripMenuItem1"
        Me.ToolStripMenuItem1.Size = New System.Drawing.Size(198, 6)
        '
        'ToolStripMenuItem7
        '
        Me.ToolStripMenuItem7.Name = "ToolStripMenuItem7"
        Me.ToolStripMenuItem7.Size = New System.Drawing.Size(201, 22)
        Me.ToolStripMenuItem7.Text = "全屏"
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        Me.ToolStripSeparator1.Size = New System.Drawing.Size(198, 6)
        '
        '第一个ToolStripMenuItem
        '
        Me.第一个ToolStripMenuItem.Name = "第一个ToolStripMenuItem"
        Me.第一个ToolStripMenuItem.Size = New System.Drawing.Size(201, 22)
        Me.第一个ToolStripMenuItem.Text = "第一个节目(&T)"
        '
        '上一个节目ToolStripMenuItem
        '
        Me.上一个节目ToolStripMenuItem.Name = "上一个节目ToolStripMenuItem"
        Me.上一个节目ToolStripMenuItem.Size = New System.Drawing.Size(201, 22)
        Me.上一个节目ToolStripMenuItem.Text = "上一个节目(&F)"
        '
        '下一个节目ToolStripMenuItem
        '
        Me.下一个节目ToolStripMenuItem.Name = "下一个节目ToolStripMenuItem"
        Me.下一个节目ToolStripMenuItem.Size = New System.Drawing.Size(201, 22)
        Me.下一个节目ToolStripMenuItem.Text = "下一个节目(&N)"
        '
        '最后一个节目ToolStripMenuItem
        '
        Me.最后一个节目ToolStripMenuItem.Name = "最后一个节目ToolStripMenuItem"
        Me.最后一个节目ToolStripMenuItem.Size = New System.Drawing.Size(201, 22)
        Me.最后一个节目ToolStripMenuItem.Text = "最后一个节目(&L)"
        '
        'ToolStripMenuItem4
        '
        Me.ToolStripMenuItem4.Name = "ToolStripMenuItem4"
        Me.ToolStripMenuItem4.Size = New System.Drawing.Size(198, 6)
        '
        '单个循环ToolStripMenuItem
        '
        Me.单个循环ToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.单曲循环ToolStripMenuItem, Me.ToolStripMenuItem14, Me.有序循环ToolStripMenuItem1, Me.无序训话ToolStripMenuItem})
        Me.单个循环ToolStripMenuItem.Name = "单个循环ToolStripMenuItem"
        Me.单个循环ToolStripMenuItem.Size = New System.Drawing.Size(201, 22)
        Me.单个循环ToolStripMenuItem.Text = "循环方式"
        '
        '单曲循环ToolStripMenuItem
        '
        Me.单曲循环ToolStripMenuItem.Name = "单曲循环ToolStripMenuItem"
        Me.单曲循环ToolStripMenuItem.Size = New System.Drawing.Size(118, 22)
        Me.单曲循环ToolStripMenuItem.Text = "单曲循环"
        '
        'ToolStripMenuItem14
        '
        Me.ToolStripMenuItem14.Name = "ToolStripMenuItem14"
        Me.ToolStripMenuItem14.Size = New System.Drawing.Size(115, 6)
        '
        '有序循环ToolStripMenuItem1
        '
        Me.有序循环ToolStripMenuItem1.Checked = True
        Me.有序循环ToolStripMenuItem1.CheckState = System.Windows.Forms.CheckState.Checked
        Me.有序循环ToolStripMenuItem1.Name = "有序循环ToolStripMenuItem1"
        Me.有序循环ToolStripMenuItem1.Size = New System.Drawing.Size(118, 22)
        Me.有序循环ToolStripMenuItem1.Text = "有序循环"
        '
        '无序训话ToolStripMenuItem
        '
        Me.无序训话ToolStripMenuItem.Name = "无序训话ToolStripMenuItem"
        Me.无序训话ToolStripMenuItem.Size = New System.Drawing.Size(118, 22)
        Me.无序训话ToolStripMenuItem.Text = "无序循环"
        '
        'ToolStripMenuItem5
        '
        Me.ToolStripMenuItem5.Name = "ToolStripMenuItem5"
        Me.ToolStripMenuItem5.Size = New System.Drawing.Size(198, 6)
        '
        'ToolStripMenuItem9
        '
        Me.ToolStripMenuItem9.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.静音MToolStripMenuItem, Me.ToolStripMenuItem12, Me.增大ToolStripMenuItem, Me.减小ToolStripMenuItem, Me.ToolStripMenuItem13, Me.最大ToolStripMenuItem, Me.居中ToolStripMenuItem, Me.最小ToolStripMenuItem})
        Me.ToolStripMenuItem9.Name = "ToolStripMenuItem9"
        Me.ToolStripMenuItem9.Size = New System.Drawing.Size(201, 22)
        Me.ToolStripMenuItem9.Text = "媒体音量(&V)"
        '
        '静音MToolStripMenuItem
        '
        Me.静音MToolStripMenuItem.Name = "静音MToolStripMenuItem"
        Me.静音MToolStripMenuItem.Size = New System.Drawing.Size(112, 22)
        Me.静音MToolStripMenuItem.Text = "静音(&M)"
        '
        'ToolStripMenuItem12
        '
        Me.ToolStripMenuItem12.Name = "ToolStripMenuItem12"
        Me.ToolStripMenuItem12.Size = New System.Drawing.Size(109, 6)
        '
        '增大ToolStripMenuItem
        '
        Me.增大ToolStripMenuItem.Name = "增大ToolStripMenuItem"
        Me.增大ToolStripMenuItem.Size = New System.Drawing.Size(112, 22)
        Me.增大ToolStripMenuItem.Text = "增大(&A)"
        '
        '减小ToolStripMenuItem
        '
        Me.减小ToolStripMenuItem.Name = "减小ToolStripMenuItem"
        Me.减小ToolStripMenuItem.Size = New System.Drawing.Size(112, 22)
        Me.减小ToolStripMenuItem.Text = "减小(&S)"
        '
        'ToolStripMenuItem13
        '
        Me.ToolStripMenuItem13.Name = "ToolStripMenuItem13"
        Me.ToolStripMenuItem13.Size = New System.Drawing.Size(109, 6)
        '
        '最大ToolStripMenuItem
        '
        Me.最大ToolStripMenuItem.Name = "最大ToolStripMenuItem"
        Me.最大ToolStripMenuItem.Size = New System.Drawing.Size(112, 22)
        Me.最大ToolStripMenuItem.Text = "最大(&U)"
        '
        '居中ToolStripMenuItem
        '
        Me.居中ToolStripMenuItem.Name = "居中ToolStripMenuItem"
        Me.居中ToolStripMenuItem.Size = New System.Drawing.Size(112, 22)
        Me.居中ToolStripMenuItem.Text = "居中(&M)"
        '
        '最小ToolStripMenuItem
        '
        Me.最小ToolStripMenuItem.Name = "最小ToolStripMenuItem"
        Me.最小ToolStripMenuItem.Size = New System.Drawing.Size(112, 22)
        Me.最小ToolStripMenuItem.Text = "最小(&D)"
        '
        '媒体声道ToolStripMenuItem
        '
        Me.媒体声道ToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.立体声ToolStripMenuItem, Me.ToolStripMenuItem6, Me.左声道ToolStripMenuItem, Me.右声道ToolStripMenuItem})
        Me.媒体声道ToolStripMenuItem.Name = "媒体声道ToolStripMenuItem"
        Me.媒体声道ToolStripMenuItem.Size = New System.Drawing.Size(201, 22)
        Me.媒体声道ToolStripMenuItem.Text = "媒体声道"
        '
        '立体声ToolStripMenuItem
        '
        Me.立体声ToolStripMenuItem.Checked = True
        Me.立体声ToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked
        Me.立体声ToolStripMenuItem.Name = "立体声ToolStripMenuItem"
        Me.立体声ToolStripMenuItem.Size = New System.Drawing.Size(106, 22)
        Me.立体声ToolStripMenuItem.Text = "立体声"
        '
        'ToolStripMenuItem6
        '
        Me.ToolStripMenuItem6.Name = "ToolStripMenuItem6"
        Me.ToolStripMenuItem6.Size = New System.Drawing.Size(103, 6)
        '
        '左声道ToolStripMenuItem
        '
        Me.左声道ToolStripMenuItem.Name = "左声道ToolStripMenuItem"
        Me.左声道ToolStripMenuItem.Size = New System.Drawing.Size(106, 22)
        Me.左声道ToolStripMenuItem.Text = "左声道"
        '
        '右声道ToolStripMenuItem
        '
        Me.右声道ToolStripMenuItem.Name = "右声道ToolStripMenuItem"
        Me.右声道ToolStripMenuItem.Size = New System.Drawing.Size(106, 22)
        Me.右声道ToolStripMenuItem.Text = "右声道"
        '
        'ToolStripMenuItem8
        '
        Me.ToolStripMenuItem8.Name = "ToolStripMenuItem8"
        Me.ToolStripMenuItem8.Size = New System.Drawing.Size(198, 6)
        '
        '显示播放列表ToolStripMenuItem
        '
        Me.显示播放列表ToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.窗体无框模式ToolStripMenuItem, Me.ToolStripMenuItem11, Me.显示播放进度控制面板PToolStripMenuItem, Me.显示播放列表ToolStripMenuItem1})
        Me.显示播放列表ToolStripMenuItem.Name = "显示播放列表ToolStripMenuItem"
        Me.显示播放列表ToolStripMenuItem.Size = New System.Drawing.Size(201, 22)
        Me.显示播放列表ToolStripMenuItem.Text = "显示"
        '
        '窗体无框模式ToolStripMenuItem
        '
        Me.窗体无框模式ToolStripMenuItem.Name = "窗体无框模式ToolStripMenuItem"
        Me.窗体无框模式ToolStripMenuItem.Size = New System.Drawing.Size(214, 22)
        Me.窗体无框模式ToolStripMenuItem.Text = "窗体无框模式"
        '
        'ToolStripMenuItem11
        '
        Me.ToolStripMenuItem11.Name = "ToolStripMenuItem11"
        Me.ToolStripMenuItem11.Size = New System.Drawing.Size(211, 6)
        '
        '显示播放进度控制面板PToolStripMenuItem
        '
        Me.显示播放进度控制面板PToolStripMenuItem.Name = "显示播放进度控制面板PToolStripMenuItem"
        Me.显示播放进度控制面板PToolStripMenuItem.Size = New System.Drawing.Size(214, 22)
        Me.显示播放进度控制面板PToolStripMenuItem.Text = "显示播放进度控制面板（&P)"
        '
        '显示播放列表ToolStripMenuItem1
        '
        Me.显示播放列表ToolStripMenuItem1.Name = "显示播放列表ToolStripMenuItem1"
        Me.显示播放列表ToolStripMenuItem1.Size = New System.Drawing.Size(214, 22)
        Me.显示播放列表ToolStripMenuItem1.Text = "显示播放列表"
        '
        'ToolStripMenuItem10
        '
        Me.ToolStripMenuItem10.Name = "ToolStripMenuItem10"
        Me.ToolStripMenuItem10.Size = New System.Drawing.Size(201, 22)
        Me.ToolStripMenuItem10.Text = "退出时清空播放列表"
        '
        'ToolStripSeparator2
        '
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        Me.ToolStripSeparator2.Size = New System.Drawing.Size(198, 6)
        '
        '窗口置顶ToolStripMenuItem
        '
        Me.窗口置顶ToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.始终置顶ToolStripMenuItem, Me.播放时置顶ToolStripMenuItem, Me.从不置顶ToolStripMenuItem})
        Me.窗口置顶ToolStripMenuItem.Name = "窗口置顶ToolStripMenuItem"
        Me.窗口置顶ToolStripMenuItem.Size = New System.Drawing.Size(201, 22)
        Me.窗口置顶ToolStripMenuItem.Text = "窗口置顶"
        '
        '始终置顶ToolStripMenuItem
        '
        Me.始终置顶ToolStripMenuItem.Name = "始终置顶ToolStripMenuItem"
        Me.始终置顶ToolStripMenuItem.Size = New System.Drawing.Size(130, 22)
        Me.始终置顶ToolStripMenuItem.Text = "始终置顶"
        '
        '播放时置顶ToolStripMenuItem
        '
        Me.播放时置顶ToolStripMenuItem.Name = "播放时置顶ToolStripMenuItem"
        Me.播放时置顶ToolStripMenuItem.Size = New System.Drawing.Size(130, 22)
        Me.播放时置顶ToolStripMenuItem.Text = "播放时置顶"
        '
        '从不置顶ToolStripMenuItem
        '
        Me.从不置顶ToolStripMenuItem.Checked = True
        Me.从不置顶ToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked
        Me.从不置顶ToolStripMenuItem.Name = "从不置顶ToolStripMenuItem"
        Me.从不置顶ToolStripMenuItem.Size = New System.Drawing.Size(130, 22)
        Me.从不置顶ToolStripMenuItem.Text = "从不置顶"
        '
        'OpenFileDialog1
        '
        Me.OpenFileDialog1.FileName = "OpenFileDialog1"
        Me.OpenFileDialog1.Filter = "媒体文件|*.mp3;*.mp4;*.wmv;*.mpg;*.mpeg;*.mov;*.avi;*.rm;*.rmvb;*.rmb;*.mkv;*.wav;*.w" &
    "ma;*.dat|所有文件|*.*"
        Me.OpenFileDialog1.Multiselect = True
        Me.OpenFileDialog1.Title = "选择媒体文件"
        '
        'Timer2
        '
        Me.Timer2.Enabled = True
        '
        'Panel1
        '
        Me.Panel1.AllowDrop = True
        Me.Panel1.Controls.Add(Me.PictureBox3)
        Me.Panel1.Controls.Add(Me.PictureBox1)
        Me.Panel1.Controls.Add(Me.PictureBox2)
        Me.Panel1.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.Panel1.Location = New System.Drawing.Point(0, 132)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(428, 8)
        Me.Panel1.TabIndex = 2
        '
        'PictureBox3
        '
        Me.PictureBox3.Cursor = System.Windows.Forms.Cursors.Hand
        Me.PictureBox3.Dock = System.Windows.Forms.DockStyle.Fill
        Me.PictureBox3.Image = Global.Player.My.Resources.Resources.e1e
        Me.PictureBox3.Location = New System.Drawing.Point(3, 0)
        Me.PictureBox3.Name = "PictureBox3"
        Me.PictureBox3.Size = New System.Drawing.Size(425, 8)
        Me.PictureBox3.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize
        Me.PictureBox3.TabIndex = 3
        Me.PictureBox3.TabStop = False
        '
        'PictureBox1
        '
        Me.PictureBox1.Dock = System.Windows.Forms.DockStyle.Left
        Me.PictureBox1.Image = Global.Player.My.Resources.Resources.q_0
        Me.PictureBox1.Location = New System.Drawing.Point(0, 0)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(3, 8)
        Me.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize
        Me.PictureBox1.TabIndex = 1
        Me.PictureBox1.TabStop = False
        '
        'PictureBox2
        '
        Me.PictureBox2.Image = Global.Player.My.Resources.Resources.q_2
        Me.PictureBox2.Location = New System.Drawing.Point(822, 0)
        Me.PictureBox2.Name = "PictureBox2"
        Me.PictureBox2.Size = New System.Drawing.Size(3, 8)
        Me.PictureBox2.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize
        Me.PictureBox2.TabIndex = 7
        Me.PictureBox2.TabStop = False
        '
        'PictureBox4
        '
        Me.PictureBox4.Cursor = System.Windows.Forms.Cursors.Hand
        Me.PictureBox4.Image = Global.Player.My.Resources.Resources.w
        Me.PictureBox4.Location = New System.Drawing.Point(-104, 389)
        Me.PictureBox4.Name = "PictureBox4"
        Me.PictureBox4.Size = New System.Drawing.Size(1589, 3)
        Me.PictureBox4.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize
        Me.PictureBox4.TabIndex = 4
        Me.PictureBox4.TabStop = False
        '
        'Timer3
        '
        Me.Timer3.Enabled = True
        '
        'Panel2
        '
        Me.Panel2.BackColor = System.Drawing.Color.Black
        Me.Panel2.Location = New System.Drawing.Point(7, 5)
        Me.Panel2.Name = "Panel2"
        Me.Panel2.Padding = New System.Windows.Forms.Padding(0, 3, 0, 0)
        Me.Panel2.Size = New System.Drawing.Size(264, 18)
        Me.Panel2.TabIndex = 11
        '
        'Timer4
        '
        Me.Timer4.Interval = 3000
        '
        'Timer5
        '
        Me.Timer5.Enabled = True
        '
        'AxWindowsMediaPlayer1
        '
        Me.AxWindowsMediaPlayer1.AllowDrop = True
        Me.AxWindowsMediaPlayer1.Enabled = True
        Me.AxWindowsMediaPlayer1.Location = New System.Drawing.Point(7, 23)
        Me.AxWindowsMediaPlayer1.Margin = New System.Windows.Forms.Padding(2)
        Me.AxWindowsMediaPlayer1.Name = "AxWindowsMediaPlayer1"
        Me.AxWindowsMediaPlayer1.OcxState = CType(resources.GetObject("AxWindowsMediaPlayer1.OcxState"), System.Windows.Forms.AxHost.State)
        Me.AxWindowsMediaPlayer1.Size = New System.Drawing.Size(264, 10)
        Me.AxWindowsMediaPlayer1.TabIndex = 10
        '
        'Form1
        '
        Me.AllowDrop = True
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 12.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(428, 140)
        Me.Controls.Add(Me.Panel2)
        Me.Controls.Add(Me.PictureBox4)
        Me.Controls.Add(Me.Panel1)
        Me.Controls.Add(Me.AxWindowsMediaPlayer1)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "Form1"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.ContextMenuStrip1.ResumeLayout(False)
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        CType(Me.PictureBox3, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PictureBox2, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PictureBox4, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.AxWindowsMediaPlayer1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Timer1 As System.Windows.Forms.Timer
    Friend WithEvents ContextMenuStrip1 As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents ToolStripMenuItem2 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem3 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents 播放暂停ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents 停止ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem1 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents 第一个ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents 上一个节目ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents 下一个节目ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents 最后一个节目ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem4 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents 单个循环ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem5 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents 媒体声道ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents 立体声ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem6 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents 左声道ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents 右声道ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem7 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator1 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents OpenFileDialog1 As System.Windows.Forms.OpenFileDialog
    Friend WithEvents ToolStripMenuItem8 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents Timer2 As System.Windows.Forms.Timer
    Friend WithEvents 窗口置顶ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents 始终置顶ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents 播放时置顶ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents 从不置顶ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents PictureBox3 As System.Windows.Forms.PictureBox
    Friend WithEvents PictureBox4 As System.Windows.Forms.PictureBox
    Friend WithEvents Timer3 As System.Windows.Forms.Timer
    Friend WithEvents 显示播放列表ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator2 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents PictureBox2 As System.Windows.Forms.PictureBox
    Friend WithEvents AxWindowsMediaPlayer1 As AxWMPLib.AxWindowsMediaPlayer
    Friend WithEvents ToolStripMenuItem10 As ToolStripMenuItem
    Friend WithEvents Panel2 As Panel
    Friend WithEvents Timer4 As Timer
    Friend WithEvents Timer5 As Timer
    Friend WithEvents 窗体无框模式ToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem11 As ToolStripSeparator
    Friend WithEvents 显示播放列表ToolStripMenuItem1 As ToolStripMenuItem
    Friend WithEvents 显示播放进度控制面板PToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem9 As ToolStripMenuItem
    Friend WithEvents 静音MToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem12 As ToolStripSeparator
    Friend WithEvents 增大ToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents 减小ToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem13 As ToolStripSeparator
    Friend WithEvents 最大ToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents 居中ToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents 最小ToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents 单曲循环ToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem14 As ToolStripSeparator
    Friend WithEvents 有序循环ToolStripMenuItem1 As ToolStripMenuItem
    Friend WithEvents 无序训话ToolStripMenuItem As ToolStripMenuItem
End Class