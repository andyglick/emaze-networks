package net.emaze.networks.my;

import net.emaze.dysfunctional.contracts.dbc;

public class MaskParsers {

    public static Mask parse(String mask) {
        dbc.precondition(mask != null, "mask must be not-null");
        if (mask.contains(".")) {
            return new IpPolicy.V4().mask(IpParsers.parseFromStringV4(mask).bits().bitCount());
        }
        throw new IllegalStateException("Cannot decide if your mask is IPv4 or IPv6");
    }

    public static Mask parseV4(String mask) {
        dbc.precondition(mask != null, "mask must be not-null");
        if (mask.contains(".")) {
            return new IpPolicy.V4().mask(IpParsers.parseFromStringV4(mask).bits().bitCount());
        }
        return new IpPolicy.V4().mask(Integer.parseInt(mask));
    }

    public static Mask parseV6(String mask) {
        dbc.precondition(mask != null, "mask must be not-null");
        return new IpPolicy.V6().mask(Integer.parseInt(mask));
    }
}
