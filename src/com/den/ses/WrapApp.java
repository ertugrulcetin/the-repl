package com.den.ses;

import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;
import java.awt.BorderLayout;

public class WrapApp extends JFrame {
    public static final String LINE_BREAK_ATTRIBUTE_NAME = "line_break_attribute";
    JEditorPane edit = new JEditorPane();

    public WrapApp() {
        super("Forsed wrap/no wrap example");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        edit.setEditorKit(new WrapEditorKit());
        initKeyMap();

        getContentPane().add(new JScrollPane(edit));
        getContentPane().add(new JLabel("Press SHIFT+ENTER to insert line break."), BorderLayout.SOUTH);
        setSize(300, 200);
        setLocationRelativeTo(null);
    }

    public static void main(String[] args) {
        WrapApp m = new WrapApp();
        m.setVisible(true);
    }

    protected void insertLineBreak() {
        try {
            int offs = edit.getCaretPosition();
            Document doc = edit.getDocument();
            SimpleAttributeSet attrs;
            if (doc instanceof StyledDocument) {
                attrs = new SimpleAttributeSet(((StyledDocument) doc).getCharacterElement(offs)
                                                                     .getAttributes());
            } else {
                attrs = new SimpleAttributeSet();
            }
            attrs.addAttribute(LINE_BREAK_ATTRIBUTE_NAME, Boolean.TRUE);
            doc.insertString(offs, "\r", attrs);
            edit.setCaretPosition(offs + 1);
        } catch (BadLocationException ex) {
            //should never happens
            ex.printStackTrace();
        }
    }

    protected void initKeyMap() {
        Keymap kMap = edit.getKeymap();
        Action a = new AbstractAction() {
            public void actionPerformed(ActionEvent e) {
                insertLineBreak();
            }
        };
        kMap.addActionForKeyStroke(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, KeyEvent.SHIFT_MASK), a);
    }
}

