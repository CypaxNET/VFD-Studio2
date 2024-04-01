/*********************************************//*!@addtogroup file Files*//*@{*/
/*!
 * @brief         Display command defines for Noritake GU 300/800 VFD series
 * 
 * @details       This file belongs to the VFD-Studio 2 project:
 *                https://github.com/CypaxNET/VFD-Studio2/
 *                
 * @copyright     Created February 2024 by Cypax, https://cypax.net
*/
/************************************************************************//*@}*/

#ifndef NTK_COMMANDS_H
#define NTK_COMMANDS_H

/******************************************************************************/
/* Public constants (#define)                                                 */
/*****************************//*!@addtogroup pubdefine Public constants*//*@{*/

/* ******** GU-800 VFD series commands ******** */
#define VFD_800_DSP_CLEAR     0x5F
#define VFD_800_AREA_SET      0x62
#define VFD_800_DSP_ONOFF     0x20
#define VFD_800_DSP_ONOFF_L0  0x04
#define VFD_800_DSP_ONOFF_L1  0x08
#define VFD_800_BRIGHTNESS    0x40
#define VFD_800_SET_X         0x64
#define VFD_800_SET_Y         0x60
#define VFD_800_ADRMODE       0x80
#define VFD_800_ADRMODE_INCX  0x04
#define VFD_800_ADRMODE_INCY  0x02

/* ******** GU-300 VFD series commands ******** */
#define VFD_300_SET_LOWER_ADDR_1  0x0A
#define VFD_300_SET_UPPER_ADDR_1  0x0B
#define VFD_300_SET_LOWER_ADDR_2  0x0C
#define VFD_300_SET_UPPER_ADDR_2  0x0D
#define VFD_300_CURSOR_INCR       0x04
#define VFD_300_CURSOR_HOLD       0x05
#define VFD_300_SCREEN1_CHARACTER 0x06
#define VFD_300_SCREEN1_GRAPHICS  0x07
#define VFD_300_DATA_WRITE        0x08
#define VFD_300_DATA_READ         0x09
#define VFD_300_SET_CURSOR_LOW    0x0E
#define VFD_300_SET_CURSOR_HIGH   0x0F
#define VFD_300_OR_DISPLAY        0x10
#define VFD_300_XOR_DISPLAY       0x11
#define VFD_300_AND_DISPLAY       0x12
#define VFD_300_SET_LUMINANCE     0x18 // full brightness

#endif // NTK_COMMANDS_H
/**** Last line of code                                                    ****/
