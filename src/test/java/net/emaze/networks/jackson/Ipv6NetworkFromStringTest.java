package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Ipv6Network;
import net.emaze.networks.jackson.Ipv6NetworkToStringTest.BeanWithCidr;
import org.junit.Test;

public class Ipv6NetworkFromStringTest {

    @Test
    public void deserializingYieldsExpected() throws IOException {
        final String serialized = "{'cidr':'0000:0000:0000:0000:0000:0000:0000:0000/64'}".replace("'", "\"");
        final Ipv6Network expected = Ipv6Network.fromCidrNotation("::/64");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithCidr got = mapper.readValue(serialized, BeanWithCidr.class);
        Assert.assertEquals(expected, got.getCidr());
    }
}
