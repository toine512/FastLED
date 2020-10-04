#ifndef __INC_PIXELS_H
#define __INC_PIXELS_H

#include "FastLED.h"

#include <stdint.h>
#include "lib8tion.h"
#include "color.h"

FASTLED_NAMESPACE_BEGIN

struct CRGB;
struct CHSV;

///@defgroup Pixeltypes CHSV and CRGB type definitions
///@{

/// Forward declaration of hsv2rgb_rainbow here,
/// to avoid circular dependencies.
struct CRGBW;
struct CHSV;

extern void hsv2rgb_rainbow( const CHSV& hsv, CRGBW& rgbw);

/// Representation of an HSV pixel (hue, saturation, value (aka brightness)).
struct CHSV {
    union {
		struct {
		    union {
		        uint8_t hue;
		        uint8_t h; };
		    union {
		        uint8_t saturation;
		        uint8_t sat;
		        uint8_t s; };
		    union {
		        uint8_t value;
		        uint8_t val;
		        uint8_t v; };
		};
		uint8_t raw[3];
	};

    /// Array access operator to index into the chsv object
	inline uint8_t& operator[] (uint8_t x) __attribute__((always_inline))
    {
        return raw[x];
    }

    /// Array access operator to index into the chsv object
    inline const uint8_t& operator[] (uint8_t x) const __attribute__((always_inline))
    {
        return raw[x];
    }

    /// default values are UNITIALIZED
    inline CHSV() __attribute__((always_inline)) = default;

    /// allow construction from H, S, V
    inline CHSV( uint8_t ih, uint8_t is, uint8_t iv) __attribute__((always_inline))
        : h(ih), s(is), v(iv)
    {
    }

    /// allow copy construction
    inline CHSV(const CHSV& rhs) __attribute__((always_inline)) = default;

    inline CHSV& operator= (const CHSV& rhs) __attribute__((always_inline)) = default;

    inline CHSV& setHSV(uint8_t ih, uint8_t is, uint8_t iv) __attribute__((always_inline))
    {
        h = ih;
        s = is;
        v = iv;
        return *this;
    }
};

/// Pre-defined hue values for HSV objects
typedef enum {
    HUE_RED = 0,
    HUE_ORANGE = 32,
    HUE_YELLOW = 64,
    HUE_GREEN = 96,
    HUE_AQUA = 128,
    HUE_BLUE = 160,
    HUE_PURPLE = 192,
    HUE_PINK = 224
} HSVHue;

/// Representation of an RGB pixel (Red, Green, Blue)
struct CRGBW {
	union {
		struct {
            union {
                uint8_t r;
                uint8_t red;
            };
            union {
                uint8_t g;
                uint8_t green;
            };
            union {
                uint8_t b;
                uint8_t blue;
            };
            union {
                uint8_t w;
                uint8_t white;
            };
        };
		uint8_t raw[4];
	};

    /// Array access operator to index into the crgb object
	inline uint8_t& operator[] (uint8_t x) __attribute__((always_inline))
    {
        return raw[x];
    }

    /// Array access operator to index into the crgb object
    inline const uint8_t& operator[] (uint8_t x) const __attribute__((always_inline))
    {
        return raw[x];
    }

    // default values are UNINITIALIZED
    inline CRGBW() __attribute__((always_inline)) = default;

    /// allow construction from R, G, B, W
    inline CRGBW( uint8_t ir, uint8_t ig, uint8_t ib, uint8_t iw)  __attribute__((always_inline))
        : r(ir), g(ig), b(ib), w(iw)
    {
    }

    /// allow construction from 32-bit (really 24-bit) bit 0xRRGGBB color code
    inline CRGBW( uint32_t colorcode)  __attribute__((always_inline))
    : r((colorcode >> 24) & 0xFF), g((colorcode >> 16) & 0xFF), b((colorcode >> 8) & 0xFF), w((colorcode >> 0) & 0xFF)
    {
    }

    /// allow construction from a LEDColorCorrection enum
    inline CRGBW( LEDColorCorrection colorcode) __attribute__((always_inline))
    : r((colorcode >> 24) & 0xFF), g((colorcode >> 16) & 0xFF), b((colorcode >> 8) & 0xFF), w((colorcode >> 0) & 0xFF)
    {

    }

    /// allow construction from a ColorTemperature enum
    inline CRGBW( ColorTemperature colorcode) __attribute__((always_inline))
    : r((colorcode >> 24) & 0xFF), g((colorcode >> 16) & 0xFF), b((colorcode >> 8) & 0xFF), w((colorcode >> 0) & 0xFF)
    {

    }

    /// allow copy construction
	inline CRGBW(const CRGBW& rhs) __attribute__((always_inline)) = default;
    /// allow construction from HSV color
	inline CRGBW(const CHSV& rhs) __attribute__((always_inline))
    {
        hsv2rgb_rainbow( rhs, *this);
    }

    /// allow assignment from one RGB struct to another
	inline CRGBW& operator= (const CRGBW& rhs) __attribute__((always_inline)) = default;

    /// allow assignment from 32-bit (really 24-bit) 0xRRGGBB color code
	inline CRGBW& operator= (const uint32_t colorcode) __attribute__((always_inline))
    {
        r = (colorcode >> 24) & 0xFF;
        g = (colorcode >> 16) & 0xFF;
        b = (colorcode >>  8) & 0xFF;
        w = (colorcode >>  0) & 0xFF;
        return *this;
    }

    /// allow assignment from R, G, and B, W
	inline CRGBW& setRGBW (uint8_t nr, uint8_t ng, uint8_t nb, uint8_t nw) __attribute__((always_inline))
    {
        r = nr;
        g = ng;
        b = nb;
        w = nw;
        return *this;
    }

    /// allow assignment from H, S, and V
	inline CRGBW& setHSV (uint8_t hue, uint8_t sat, uint8_t val) __attribute__((always_inline))
    {
        hsv2rgb_rainbow( CHSV(hue, sat, val), *this);
        return *this;
    }

    /// allow assignment from just a Hue, saturation and value automatically at max.
	inline CRGBW& setHue (uint8_t hue) __attribute__((always_inline))
    {
        hsv2rgb_rainbow( CHSV(hue, 255, 255), *this);
        return *this;
    }

    /// allow assignment from HSV color
	inline CRGBW& operator= (const CHSV& rhs) __attribute__((always_inline))
    {
        hsv2rgb_rainbow( rhs, *this);
        return *this;
    }

    /// allow assignment from 32-bit (really 24-bit) 0xRRGGBB color code
	inline CRGBW& setColorCode (uint32_t colorcode) __attribute__((always_inline))
    {
        r = (colorcode >> 24) & 0xFF;
        g = (colorcode >> 16) & 0xFF;
        b = (colorcode >>  8) & 0xFF;
        w = (colorcode >>  0) & 0xFF;
        return *this;
    }


    /// add one RGB to another, saturating at 0xFF for each channel
    inline CRGBW& operator+= (const CRGBW& rhs )
    {
        r = qadd8( r, rhs.r);
        g = qadd8( g, rhs.g);
        b = qadd8( b, rhs.b);
        w = qadd8( w, rhs.w);
        return *this;
    }

    /// add a contstant to each channel, saturating at 0xFF
    /// this is NOT an operator+= overload because the compiler
    /// can't usefully decide when it's being passed a 32-bit
    /// constant (e.g. CRGBW::Red) and an 8-bit one (CRGBW::Blue)
    inline CRGBW& addToRGBW (uint8_t d )
    {
        r = qadd8( r, d);
        g = qadd8( g, d);
        b = qadd8( b, d);
        w = qadd8( w, d);
        return *this;
    }

    /// subtract one RGB from another, saturating at 0x00 for each channel
    inline CRGBW& operator-= (const CRGBW& rhs )
    {
        r = qsub8( r, rhs.r);
        g = qsub8( g, rhs.g);
        b = qsub8( b, rhs.b);
        w = qsub8( w, rhs.w);
        return *this;
    }

    /// subtract a constant from each channel, saturating at 0x00
    /// this is NOT an operator+= overload because the compiler
    /// can't usefully decide when it's being passed a 32-bit
    /// constant (e.g. CRGBW::Red) and an 8-bit one (CRGBW::Blue)
    inline CRGBW& subtractFromRGBW(uint8_t d )
    {
        r = qsub8( r, d);
        g = qsub8( g, d);
        b = qsub8( b, d);
        w = qsub8( w, d);
        return *this;
    }

    /// subtract a constant of '1' from each channel, saturating at 0x00
    inline CRGBW& operator-- ()  __attribute__((always_inline))
    {
        subtractFromRGBW(1);
        return *this;
    }

    /// subtract a constant of '1' from each channel, saturating at 0x00
    inline CRGBW operator-- (int )  __attribute__((always_inline))
    {
        CRGBW retval(*this);
        --(*this);
        return retval;
    }

    /// add a constant of '1' from each channel, saturating at 0xFF
    inline CRGBW& operator++ ()  __attribute__((always_inline))
    {
        addToRGBW(1);
        return *this;
    }

    /// add a constant of '1' from each channel, saturating at 0xFF
    inline CRGBW operator++ (int )  __attribute__((always_inline))
    {
        CRGBW retval(*this);
        ++(*this);
        return retval;
    }

    /// divide each of the channels by a constant
    inline CRGBW& operator/= (uint8_t d )
    {
        r /= d;
        g /= d;
        b /= d;
        w /= d;
        return *this;
    }

    /// right shift each of the channels by a constant
    inline CRGBW& operator>>= (uint8_t d)
    {
        r >>= d;
        g >>= d;
        b >>= d;
        w >>= d;
        return *this;
    }

    /// multiply each of the channels by a constant,
    /// saturating each channel at 0xFF
    inline CRGBW& operator*= (uint8_t d )
    {
        r = qmul8( r, d);
        g = qmul8( g, d);
        b = qmul8( b, d);
        w = qmul8( w, d);
        return *this;
    }

    /// scale down a RGB to N 256ths of it's current brightness, using
    /// 'video' dimming rules, which means that unless the scale factor is ZERO
    /// each channel is guaranteed NOT to dim down to zero.  If it's already
    /// nonzero, it'll stay nonzero, even if that means the hue shifts a little
    /// at low brightness levels.
    inline CRGBW& nscale8_video (uint8_t scaledown )
    {
        nscale8x3_video( r, g, b, w, scaledown);
        return *this;
    }

    /// %= is a synonym for nscale8_video.  Think of it is scaling down
    /// by "a percentage"
    inline CRGBW& operator%= (uint8_t scaledown )
    {
        nscale8x3_video( r, g, b, w, scaledown);
        return *this;
    }

    /// fadeLightBy is a synonym for nscale8_video( ..., 255-fadefactor)
    inline CRGBW& fadeLightBy (uint8_t fadefactor )
    {
        nscale8x3_video( r, g, b, w, 255 - fadefactor);
        return *this;
    }

    /// scale down a RGB to N 256ths of it's current brightness, using
    /// 'plain math' dimming rules, which means that if the low light levels
    /// may dim all the way to 100% black.
    inline CRGBW& nscale8 (uint8_t scaledown )
    {
        nscale8x3( r, g, b, w, scaledown);
        return *this;
    }

    /// scale down a RGB to N 256ths of it's current brightness, using
    /// 'plain math' dimming rules, which means that if the low light levels
    /// may dim all the way to 100% black.
    inline CRGBW& nscale8 (const CRGBW & scaledown )
    {
        r = ::scale8(r, scaledown.r);
        g = ::scale8(g, scaledown.g);
        b = ::scale8(b, scaledown.b);
        w = ::scale8(w, scaledown.w);
        return *this;
    }

    /// return a CRGBW object that is a scaled down version of this object
    inline CRGBW scale8 (const CRGBW & scaledown ) const
    {
        CRGBW out;
        out.r = ::scale8(r, scaledown.r);
        out.g = ::scale8(g, scaledown.g);
        out.b = ::scale8(b, scaledown.b);
        out.w = ::scale8(w, scaledown.w);
        return out;
    }

    /// fadeToBlackBy is a synonym for nscale8( ..., 255-fadefactor)
    inline CRGBW& fadeToBlackBy (uint8_t fadefactor )
    {
        nscale8x3( r, g, b, w, 255 - fadefactor);
        return *this;
    }

    /// "or" operator brings each channel up to the higher of the two values
    inline CRGBW& operator|= (const CRGBW& rhs )
    {
        if( rhs.r > r) r = rhs.r;
        if( rhs.g > g) g = rhs.g;
        if( rhs.b > b) b = rhs.b;
        if( rhs.w > w) w = rhs.w;
        return *this;
    }

    /// "or" operator brings each channel up to the higher of the two values
    inline CRGBW& operator|= (uint8_t d )
    {
        if( d > r) r = d;
        if( d > g) g = d;
        if( d > b) b = d;
        if( d > w) w = d;
        return *this;
    }

    /// "and" operator brings each channel down to the lower of the two values
    inline CRGBW& operator&= (const CRGBW& rhs )
    {
        if( rhs.r < r) r = rhs.r;
        if( rhs.g < g) g = rhs.g;
        if( rhs.b < b) b = rhs.b;
        if( rhs.w < w) w = rhs.w;
        return *this;
    }

    /// "and" operator brings each channel down to the lower of the two values
    inline CRGBW& operator&= (uint8_t d )
    {
        if( d < r) r = d;
        if( d < g) g = d;
        if( d < b) b = d;
        if( d < w) w = d;
        return *this;
    }

    /// this allows testing a CRGBW for zero-ness
    inline operator bool() const __attribute__((always_inline))
    {
        return r || g || b || w;
    }

    /// invert each channel
    inline CRGBW operator- ()
    {
        CRGBW retval;
        retval.r = 255 - r;
        retval.g = 255 - g;
        retval.b = 255 - b;
        retval.w = 255 - w;
        return retval;
    }

#if (defined SmartMatrix_h || defined SmartMatrix3_h)
    operator rgb24() const {
        rgb24 ret;
        ret.red = r;
        ret.green = g;
        ret.blue = b;
        return ret;
    }
#endif

    /// Get the 'luma' of a CRGBW object - aka roughly how much light the
    /// CRGBW pixel is putting out (from 0 to 255).
    inline uint8_t getLuma ( )  const {
        //Y' = 0.2126 R' + 0.7152 G' + 0.0722 B'
        //     54            183       18 (!)

        uint8_t luma = scale8_LEAVING_R1_DIRTY( r, 54) + \
        scale8_LEAVING_R1_DIRTY( g, 183) + \
        scale8_LEAVING_R1_DIRTY( b, 18);
        cleanup_R1();
        return luma;
    }

    /// Get the average of the R, G, and B values
    inline uint8_t getAverageLight( )  const {
#if FASTLED_SCALE8_FIXED == 1
        const uint8_t eightyfive = 85;
#else
        const uint8_t eightyfive = 86;
#endif
        uint8_t avg = scale8_LEAVING_R1_DIRTY( r, eightyfive) + \
        scale8_LEAVING_R1_DIRTY( g, eightyfive) + \
        scale8_LEAVING_R1_DIRTY( b, eightyfive);
        cleanup_R1();
        return avg;
    }

    /// maximize the brightness of this CRGBW object
    inline void maximizeBrightness( uint8_t limit = 255 )  {
        uint8_t max = red;
        if( green > max) max = green;
        if( blue > max) max = blue;
        if( white > max) max = white;

        // stop div/0 when color is black
        if(max > 0) {
            uint16_t factor = ((uint16_t)(limit) * 256) / max;
            red =   (red   * factor) / 256;
            green = (green * factor) / 256;
            blue =  (blue  * factor) / 256;
            white =  (white  * factor) / 256;
        }
    }

    /// return a new CRGBW object after performing a linear interpolation between this object and the passed in object
    inline CRGBW lerp8( const CRGBW& other, fract8 frac) const
    {
        CRGBW ret;

        ret.r = lerp8by8(r,other.r,frac);
        ret.g = lerp8by8(g,other.g,frac);
        ret.b = lerp8by8(b,other.b,frac);
        ret.w = lerp8by8(w,other.w,frac);

        return ret;
    }

    /// return a new CRGBW object after performing a linear interpolation between this object and the passed in object
    inline CRGBW lerp16( const CRGBW& other, fract16 frac) const
    {
        CRGBW ret;

        ret.r = lerp16by16(r<<8,other.r<<8,frac)>>8;
        ret.g = lerp16by16(g<<8,other.g<<8,frac)>>8;
        ret.b = lerp16by16(b<<8,other.b<<8,frac)>>8;
        ret.w = lerp16by16(w<<8,other.w<<8,frac)>>8;

        return ret;
    }

    /// getParity returns 0 or 1, depending on the
    /// lowest bit of the sum of the color components.
    inline uint8_t getParity()
    {
        uint8_t sum = r + g + b + w;
        return (sum & 0x01);
    }

    /// setParity adjusts the color in the smallest
    /// way possible so that the parity of the color
    /// is now the desired value.  This allows you to
    /// 'hide' one bit of information in the color.
    ///
    /// Ideally, we find one color channel which already
    /// has data in it, and modify just that channel by one.
    /// We don't want to light up a channel that's black
    /// if we can avoid it, and if the pixel is 'grayscale',
    /// (meaning that R==G==B), we modify all three channels
    /// at once, to preserve the neutral hue.
    ///
    /// There's no such thing as a free lunch; in many cases
    /// this 'hidden bit' may actually be visible, but this
    /// code makes reasonable efforts to hide it as much
    /// as is reasonably possible.
    ///
    /// Also, an effort is made to have make it such that
    /// repeatedly setting the parity to different values
    /// will not cause the color to 'drift'.  Toggling
    /// the parity twice should generally result in the
    /// original color again.
    ///
    inline void setParity( uint8_t parity)
    {
        uint8_t curparity = getParity();

        if( parity == curparity) return;

        if( parity ) {
            // going 'up'
            if( (b > 0) && (b < 255)) {
                if( r == g && g == b) {
                    r++;
                    g++;
                }
                b++;
            } else if( (r > 0) && (r < 255)) {
                r++;
            } else if( (g > 0) && (g < 255)) {
                g++;
            } else {
                if( r == g && g == b) {
                    r ^= 0x01;
                    g ^= 0x01;
                }
                b ^= 0x01;
            }
        } else {
            // going 'down'
            if( b > 1) {
                if( r == g && g == b) {
                    r--;
                    g--;
                }
                b--;
            } else if( g > 1) {
                g--;
            } else if( r > 1) {
                r--;
            } else {
                if( r == g && g == b) {
                    r ^= 0x01;
                    g ^= 0x01;
                }
                b ^= 0x01;
            }
        }
    }

    /// Predefined RGB colors
    typedef enum {
        AliceBlue=0xF0F8FF00,
        Amethyst=0x9966CC00,
        AntiqueWhite=0xFAEBD700,
        Aqua=0x00FFFF00,
        Aquamarine=0x7FFFD400,
        Azure=0xF0FFFF00,
        Beige=0xF5F5DC00,
        Bisque=0xFFE4C400,
        Black=0x00000000,
        BlanchedAlmond=0xFFEBCD00,
        Blue=0x0000FF00,
        BlueViolet=0x8A2BE200,
        Brown=0xA52A2A00,
        BurlyWood=0xDEB88700,
        CadetBlue=0x5F9EA000,
        Chartreuse=0x7FFF0000,
        Chocolate=0xD2691E00,
        Coral=0xFF7F5000,
        CornflowerBlue=0x6495ED00,
        Cornsilk=0xFFF8DC00,
        Crimson=0xDC143C00,
        Cyan=0x00FFFF00,
        DarkBlue=0x00008B00,
        DarkCyan=0x008B8B00,
        DarkGoldenrod=0xB8860B00,
        DarkGray=0xA9A9A900,
        DarkGrey=0xA9A9A900,
        DarkGreen=0x00640000,
        DarkKhaki=0xBDB76B00,
        DarkMagenta=0x8B008B00,
        DarkOliveGreen=0x556B2F00,
        DarkOrange=0xFF8C0000,
        DarkOrchid=0x9932CC00,
        DarkRed=0x8B000000,
        DarkSalmon=0xE9967A00,
        DarkSeaGreen=0x8FBC8F00,
        DarkSlateBlue=0x483D8B00,
        DarkSlateGray=0x2F4F4F00,
        DarkSlateGrey=0x2F4F4F00,
        DarkTurquoise=0x00CED100,
        DarkViolet=0x9400D300,
        DeepPink=0xFF149300,
        DeepSkyBlue=0x00BFFF00,
        DimGray=0x69696900,
        DimGrey=0x69696900,
        DodgerBlue=0x1E90FF00,
        FireBrick=0xB2222200,
        FloralWhite=0xFFFAF000,
        ForestGreen=0x228B2200,
        Fuchsia=0xFF00FF00,
        Gainsboro=0xDCDCDC00,
        GhostWhite=0xF8F8FF00,
        Gold=0xFFD70000,
        Goldenrod=0xDAA52000,
        Gray=0x80808000,
        Grey=0x80808000,
        Green=0x00800000,
        GreenYellow=0xADFF2F00,
        Honeydew=0xF0FFF000,
        HotPink=0xFF69B400,
        IndianRed=0xCD5C5C00,
        Indigo=0x4B008200,
        Ivory=0xFFFFF000,
        Khaki=0xF0E68C00,
        Lavender=0xE6E6FA00,
        LavenderBlush=0xFFF0F500,
        LawnGreen=0x7CFC0000,
        LemonChiffon=0xFFFACD00,
        LightBlue=0xADD8E600,
        LightCoral=0xF0808000,
        LightCyan=0xE0FFFF00,
        LightGoldenrodYellow=0xFAFAD200,
        LightGreen=0x90EE9000,
        LightGrey=0xD3D3D300,
        LightPink=0xFFB6C100,
        LightSalmon=0xFFA07A00,
        LightSeaGreen=0x20B2AA00,
        LightSkyBlue=0x87CEFA00,
        LightSlateGray=0x77889900,
        LightSlateGrey=0x77889900,
        LightSteelBlue=0xB0C4DE00,
        LightYellow=0xFFFFE000,
        Lime=0x00FF0000,
        LimeGreen=0x32CD3200,
        Linen=0xFAF0E600,
        Magenta=0xFF00FF00,
        Maroon=0x80000000,
        MediumAquamarine=0x66CDAA00,
        MediumBlue=0x0000CD00,
        MediumOrchid=0xBA55D300,
        MediumPurple=0x9370DB00,
        MediumSeaGreen=0x3CB37100,
        MediumSlateBlue=0x7B68EE00,
        MediumSpringGreen=0x00FA9A00,
        MediumTurquoise=0x48D1CC00,
        MediumVioletRed=0xC7158500,
        MidnightBlue=0x19197000,
        MintCream=0xF5FFFA00,
        MistyRose=0xFFE4E100,
        Moccasin=0xFFE4B500,
        NavajoWhite=0xFFDEAD00,
        Navy=0x00008000,
        OldLace=0xFDF5E600,
        Olive=0x80800000,
        OliveDrab=0x6B8E2300,
        Orange=0xFFA50000,
        OrangeRed=0xFF450000,
        Orchid=0xDA70D600,
        PaleGoldenrod=0xEEE8AA00,
        PaleGreen=0x98FB9800,
        PaleTurquoise=0xAFEEEE00,
        PaleVioletRed=0xDB709300,
        PapayaWhip=0xFFEFD500,
        PeachPuff=0xFFDAB900,
        Peru=0xCD853F00,
        Pink=0xFFC0CB00,
        Plaid=0xCC553300,
        Plum=0xDDA0DD00,
        PowderBlue=0xB0E0E600,
        Purple=0x80008000,
        Red=0xFF000000,
        RosyBrown=0xBC8F8F00,
        RoyalBlue=0x4169E100,
        SaddleBrown=0x8B451300,
        Salmon=0xFA807200,
        SandyBrown=0xF4A46000,
        SeaGreen=0x2E8B5700,
        Seashell=0xFFF5EE00,
        Sienna=0xA0522D00,
        Silver=0xC0C0C000,
        SkyBlue=0x87CEEB00,
        SlateBlue=0x6A5ACD00,
        SlateGray=0x70809000,
        SlateGrey=0x70809000,
        Snow=0xFFFAFA00,
        SpringGreen=0x00FF7F00,
        SteelBlue=0x4682B400,
        Tan=0xD2B48C00,
        Teal=0x00808000,
        Thistle=0xD8BFD800,
        Tomato=0xFF634700,
        Turquoise=0x40E0D000,
        Violet=0xEE82EE00,
        Wheat=0xF5DEB300,
        White=0xFFFFFF00,
        WhiteSmoke=0xF5F5F500,
        Yellow=0xFFFF0000,
        YellowGreen=0x9ACD3200,

        // LED RGB color that roughly approximates
        // the color of incandescent fairy lights,
        // assuming that you're using FastLED
        // color correction on your LEDs (recommended).
        FairyLight=0xFFE42D00,
        // If you are using no color correction, use this
        FairyLightNCC=0xFF9D2A00,

        // White LED (phosphor)
        WhiteLight=0x000000FF

    } HTMLColorCode;
};


inline __attribute__((always_inline)) bool operator== (const CRGBW& lhs, const CRGBW& rhs)
{
    return (lhs.r == rhs.r) && (lhs.g == rhs.g) && (lhs.b == rhs.b) && (lhs.w == rhs.w);
}

inline __attribute__((always_inline)) bool operator!= (const CRGBW& lhs, const CRGBW& rhs)
{
    return !(lhs == rhs);
}

inline __attribute__((always_inline)) bool operator< (const CRGBW& lhs, const CRGBW& rhs)
{
    uint16_t sl, sr;
    sl = lhs.r + lhs.g + lhs.b + lhs.w;
    sr = rhs.r + rhs.g + rhs.b + rhs.w;
    return sl < sr;
}

inline __attribute__((always_inline)) bool operator> (const CRGBW& lhs, const CRGBW& rhs)
{
    uint16_t sl, sr;
    sl = lhs.r + lhs.g + lhs.b + lhs.w;
    sr = rhs.r + rhs.g + rhs.b + rhs.w;
    return sl > sr;
}

inline __attribute__((always_inline)) bool operator>= (const CRGBW& lhs, const CRGBW& rhs)
{
    uint16_t sl, sr;
    sl = lhs.r + lhs.g + lhs.b + lhs.w;
    sr = rhs.r + rhs.g + rhs.b + rhs.w;
    return sl >= sr;
}

inline __attribute__((always_inline)) bool operator<= (const CRGBW& lhs, const CRGBW& rhs)
{
    uint16_t sl, sr;
    sl = lhs.r + lhs.g + lhs.b + lhs.w;
    sr = rhs.r + rhs.g + rhs.b + rhs.w;
    return sl <= sr;
}


__attribute__((always_inline))
inline CRGBW operator+( const CRGBW& p1, const CRGBW& p2)
{
    return CRGBW( qadd8( p1.r, p2.r),
                 qadd8( p1.g, p2.g),
                 qadd8( p1.b, p2.b),
                 qadd8( p1.w, p2.w));
}

__attribute__((always_inline))
inline CRGBW operator-( const CRGBW& p1, const CRGBW& p2)
{
    return CRGBW( qsub8( p1.r, p2.r),
                 qsub8( p1.g, p2.g),
                 qsub8( p1.b, p2.b),
                 qsub8( p1.w, p2.w));
}

__attribute__((always_inline))
inline CRGBW operator*( const CRGBW& p1, uint8_t d)
{
    return CRGBW( qmul8( p1.r, d),
                 qmul8( p1.g, d),
                 qmul8( p1.b, d),
                 qmul8( p1.w, d));
}

__attribute__((always_inline))
inline CRGBW operator/( const CRGBW& p1, uint8_t d)
{
    return CRGBW( p1.r/d, p1.g/d, p1.b/d, p1.w/d);
}


__attribute__((always_inline))
inline CRGBW operator&( const CRGBW& p1, const CRGBW& p2)
{
    return CRGBW( p1.r < p2.r ? p1.r : p2.r,
                 p1.g < p2.g ? p1.g : p2.g,
                 p1.b < p2.b ? p1.b : p2.b,
                 p1.w < p2.w ? p1.w : p2.w);
}

__attribute__((always_inline))
inline CRGBW operator|( const CRGBW& p1, const CRGBW& p2)
{
    return CRGBW( p1.r > p2.r ? p1.r : p2.r,
                 p1.g > p2.g ? p1.g : p2.g,
                 p1.b > p2.b ? p1.b : p2.b,
                 p1.w > p2.w ? p1.w : p2.w);
}

__attribute__((always_inline))
inline CRGBW operator%( const CRGBW& p1, uint8_t d)
{
    CRGBW retval( p1);
    retval.nscale8_video( d);
    return retval;
}



/// RGB orderings, used when instantiating controllers to determine what
/// order the controller should send RGB data out in, RGB being the default
/// ordering.
enum EOrder {
	RGB=00123,
	RBG=00213,
	GRB=01023,
	GBR=01203,
	BRG=02013,
	BGR=02103
};

FASTLED_NAMESPACE_END
///@}

#endif
