package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import net.emaze.networks.Ipv6Network;
import org.junit.Assert;
import org.junit.Test;

public class Ipv6NetworkFromStringTest {

    @Test
    public void deserializingIPv6NetworkYieldsExpected() throws IOException {
        final String serialized = "{'cidr':'1234:0000:0000:0000:0000:0000:0000:0000/16'}".replace("'", "\"");
        final Ipv6Network expected = Ipv6Network.fromCidrNotation("1234::/16");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIpv6Cidr got = mapper.readValue(serialized, BeanWithIpv6Cidr.class);
        Assert.assertEquals(expected, got.getCidr());
    }
}
