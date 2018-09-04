package com.den.ses;

import javax.swing.text.AbstractDocument;
import javax.swing.text.BoxView;
import javax.swing.text.ComponentView;
import javax.swing.text.Element;
import javax.swing.text.IconView;
import javax.swing.text.LabelView;
import javax.swing.text.StyleConstants;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;
import java.awt.BorderLayout;

public class WrapEditorKit extends StyledEditorKit {
    ViewFactory defaultFactory = new WrapColumnFactory();

    public ViewFactory getViewFactory() {
        return defaultFactory;
    }

    public MutableAttributeSet getInputAttributes() {
        MutableAttributeSet mAttrs = super.getInputAttributes();
        mAttrs.removeAttribute(WrapApp.LINE_BREAK_ATTRIBUTE_NAME);
        return mAttrs;
    }


    class WrapColumnFactory implements ViewFactory {
        public View create(Element elem) {
            String kind = elem.getName();
            if (kind != null) {
                if (kind.equals(AbstractDocument.ContentElementName)) {
                    return new WrapLabelView(elem);
                } else if (kind.equals(AbstractDocument.ParagraphElementName)) {
                    return new NoWrapParagraphView(elem);
                } else if (kind.equals(AbstractDocument.SectionElementName)) {
                    return new BoxView(elem, View.Y_AXIS);
                } else if (kind.equals(StyleConstants.ComponentElementName)) {
                    return new ComponentView(elem);
                } else if (kind.equals(StyleConstants.IconElementName)) {
                    return new IconView(elem);
                }
            }

            // default to text display
            return new LabelView(elem);
        }
    }

    class NoWrapParagraphView extends ParagraphView {
        public NoWrapParagraphView(Element elem) {
            super(elem);
        }

        public void layout(int width, int height) {
            super.layout(Short.MAX_VALUE, height);
        }

        public float getMinimumSpan(int axis) {
            return super.getPreferredSpan(axis);
        }
    }

    class WrapLabelView extends LabelView {
        public WrapLabelView(Element elem) {
            super(elem);
        }

        public int getBreakWeight(int axis, float pos, float len) {
            if (axis == View.X_AXIS) {
                checkPainter();
                int p0 = getStartOffset();
                int p1 = getGlyphPainter().getBoundedPosition(this, p0, pos, len);
                if (p1 == p0) {
                    // can't even fit a single character
                    return View.BadBreakWeight;
                }
                try {
                    //if the view contains line break char return forced break
                    if (getDocument().getText(p0, p1 - p0)
                                     .indexOf("\r") >= 0) {
                        return View.ForcedBreakWeight;
                    }
                } catch (BadLocationException ex) {
                    //should never happen
                }
            }
            return super.getBreakWeight(axis, pos, len);
        }

        public View breakView(int axis, int p0, float pos, float len) {
            if (axis == View.X_AXIS) {
                checkPainter();
                int p1 = getGlyphPainter().getBoundedPosition(this, p0, pos, len);
                try {
                    //if the view contains line break char break the view
                    int index = getDocument().getText(p0, p1 - p0)
                                             .indexOf("\r");
                    if (index >= 0) {
                        GlyphView v = (GlyphView) createFragment(p0, p0 + index + 1);
                        return v;
                    }
                } catch (BadLocationException ex) {
                    //should never happen
                }
            }
            return super.breakView(axis, p0, pos, len);
        }
    }
}