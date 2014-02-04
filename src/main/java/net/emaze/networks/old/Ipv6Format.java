package net.emaze.networks.old;

import java.util.ArrayList;
import java.util.List;
import net.emaze.dysfunctional.Strings;

public final class Ipv6Format {

    private Ipv6Format() {
    }

    public static String toNormalForm(String ip) {
        return new Ipv6FormatToNormalForm().perform(ip);
    }

    public static String toCompressedForm(String ip) {
        return new Ipv6FormatToCompressedForm().perform(ip);
    }

    public static String mask(int size) {
        final List<String> pieces = new ArrayList<>();
        for (int piece : Ipv6Mask.net(size).pieces()) {
            pieces.add(String.format("%04x", piece));
        }
        return Strings.interpose(pieces, ":");
    }
}
