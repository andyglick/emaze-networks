package net.emaze.networks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import net.emaze.dysfunctional.Strings;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;

public class Ipv6FormatToNormalForm implements Delegate<String, String> {

    @Override
    public String perform(String ip) {
        dbc.precondition(ip.indexOf("::") == ip.lastIndexOf("::"), "Not compliant IP form, at least one occurence of T_PAAMAYIM_NEKUDOTAYIM allowed");
        if (ip.startsWith("::")) {
            ip = "0000" + ip;
        }
        if (ip.endsWith("::")) {
            ip += "0000";
        }
        ip = insertMissingZeroGroups(ip);
        return insertPaddingZeroes(ip).toLowerCase();
    }

    private String insertMissingZeroGroups(String ip) {
        final String[] halves = ip.split("::");
        if (halves.length == 2) {
            final int missing = 8 - (halves[0].split(":").length + halves[1].split(":").length);
            final String missingZeros = Strings.interpose(Collections.nCopies(missing, "0000"), ":");
            return Strings.interpose(new String[]{halves[0], missingZeros, halves[1]}, ":");
        }
        return ip;
    }

    private String insertPaddingZeroes(String ip) {
        final String[] groups = ip.split(":");
        for (int i = 0; i < groups.length; i++) {
            groups[i] = Strings.join(Collections.nCopies(4 - groups[i].length(), "0")) + groups[i];
        }
        return Strings.interpose(groups, ":");
    }
}
